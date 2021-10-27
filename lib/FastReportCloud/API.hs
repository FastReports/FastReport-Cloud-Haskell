{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module FastReportCloud.API
  -- * Client and Server
  ( Config(..)
  , FastReportCloudBackend(..)
  , createFastReportCloudClient
  , runFastReportCloudServer
  , runFastReportCloudMiddlewareServer
  , runFastReportCloudClient
  , runFastReportCloudClientWithManager
  , callFastReportCloud
  , FastReportCloudClient
  , FastReportCloudClientError(..)
  -- ** Servant
  , FastReportCloudAPI
  ) where

import           FastReportCloud.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application)
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for FastReportCloud.
type FastReportCloudAPI
    =    "api" :> "manage" :> "v1" :> "ApiKeys" :> ReqBody '[JSON] CreateApiKeyVM :> Verb 'POST 200 '[JSON] ApiKeyVM -- 'apiKeysCreateApiKey' route
    :<|> "api" :> "manage" :> "v1" :> "ApiKeys" :> ReqBody '[JSON] DeleteApiKeyVM :> Verb 'DELETE 200 '[JSON] () -- 'apiKeysDeleteApiKey' route
    :<|> "api" :> "manage" :> "v1" :> "ApiKeys" :> Verb 'GET 200 '[JSON] ApiKeysVM -- 'apiKeysGetApiKeys' route
    :<|> "api" :> "v1" :> "Configuration" :> Verb 'GET 200 '[JSON] ServerConfigurationVM -- 'configurationGet' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> ReqBody '[JSON] CreateDataSourceVM :> Verb 'POST 200 '[JSON] DataSourceVM -- 'dataSourcesCreateDataSource' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] () -- 'dataSourcesDeleteDataSource' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "fetch" :> Verb 'GET 200 '[JSON] () -- 'dataSourcesFetchData' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> QueryParam "subscriptionId" Text :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" DataSourceSorting :> QueryParam "desc" Bool :> Verb 'GET 200 '[JSON] DataSourcesVM -- 'dataSourcesGetAvailableDataSources' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> Verb 'GET 200 '[JSON] DataSourceVM -- 'dataSourcesGetDataSource' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] DataSourcePermissionsVM -- 'dataSourcesGetPermissions' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "rename" :> ReqBody '[JSON] RenameDataSourceVM :> Verb 'PUT 200 '[JSON] DataSourceVM -- 'dataSourcesRenameDataSource' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "ConnectionString" :> ReqBody '[JSON] UpdateDataSourceConnectionStringVM :> Verb 'PUT 200 '[JSON] DataSourceVM -- 'dataSourcesUpdateConnectionString' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateDataSourcePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'dataSourcesUpdatePermissions' route
    :<|> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "updateSubscription" :> ReqBody '[JSON] UpdateDataSourceSubscriptionVM :> Verb 'PUT 200 '[JSON] () -- 'dataSourcesUpdateSubscriptionDataSource' route
    :<|> "download" :> "e" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExport' route
    :<|> "download" :> "e" :> Capture "id" Text :> "thumbnail" :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExportThumbnail' route
    :<|> "download" :> "es" :> Capture "archiveName" Text :> QueryParam "ids" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExports' route
    :<|> "download" :> "r" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReport' route
    :<|> "download" :> "r" :> Capture "id" Text :> "thumbnail" :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReportThumbnail' route
    :<|> "download" :> "rs" :> Capture "archiveName" Text :> QueryParam "ids" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReports' route
    :<|> "download" :> "t" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetTemplate' route
    :<|> "download" :> "ts" :> Capture "archiveName" Text :> QueryParam "ids" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetTemplates' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'exportFolderAndFileGetCount' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> Verb 'GET 200 '[JSON] FilesVM -- 'exportFolderAndFileGetFoldersAndFiles' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersCopyFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> QueryParam "recursive" Bool :> Verb 'DELETE 200 '[JSON] () -- 'exportFoldersDeleteFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'exportFoldersGetBreadcrumbs' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'exportFoldersGetFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] FilesVM -- 'exportFoldersGetFolders' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'exportFoldersGetFoldersCount' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'exportFoldersGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'exportFoldersGetRootFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersMoveFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] ExportFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersPostFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersRenameFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'exportFoldersUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersUpdateTags' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ExportVM -- 'exportsCopyFile' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] () -- 'exportsDeleteFile' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ExportVM -- 'exportsGetFile' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'exportsGetFilesCount' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] ExportsVM -- 'exportsGetFilesList' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'exportsGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ExportVM -- 'exportsMoveFile' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsRenameFile' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'exportsUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsUpdateTags' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> Capture "userId" Text :> Verb 'PUT 200 '[JSON] () -- 'groupUsersAddUserToGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] GroupUsersVM -- 'groupUsersGetUsersInGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "leave" :> Verb 'DELETE 200 '[JSON] () -- 'groupUsersLeaveFromGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> Capture "userId" Text :> Verb 'DELETE 200 '[JSON] () -- 'groupUsersRemoveFromGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> ReqBody '[JSON] CreateGroupVM :> Verb 'POST 200 '[JSON] GroupVM -- 'groupsCreateGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] () -- 'groupsDeleteGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> Verb 'GET 200 '[JSON] GroupVM -- 'groupsGetGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] GroupsVM -- 'groupsGetGroupList' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] GroupPermissionsVM -- 'groupsGetPermissions' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "rename" :> ReqBody '[JSON] RenameGroupVM :> Verb 'PUT 200 '[JSON] GroupVM -- 'groupsRenameGroup' route
    :<|> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateGroupPermissionsVM :> Verb 'POST 200 '[JSON] () -- 'groupsUpdatePermissions' route
    :<|> "api" :> "backend" :> "v1" :> "HealthCheck" :> Verb 'GET 200 '[JSON] () -- 'healthCheckDataGet' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'reportFolderAndFileGetCount' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> Verb 'GET 200 '[JSON] FilesVM -- 'reportFolderAndFileGetFoldersAndFiles' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersCopyFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> QueryParam "recursive" Bool :> Verb 'DELETE 200 '[JSON] () -- 'reportFoldersDeleteFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'reportFoldersGetBreadcrumbs' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'reportFoldersGetFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] FilesVM -- 'reportFoldersGetFolders' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'reportFoldersGetFoldersCount' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'reportFoldersGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'reportFoldersGetRootFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersMoveFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] ReportFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersPostFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersRenameFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'reportFoldersUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersUpdateTags' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsCopyFile' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] () -- 'reportsDeleteFile' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportReportVM :> Verb 'POST 200 '[JSON] ExportVM -- 'reportsExport' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ReportVM -- 'reportsGetFile' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'reportsGetFilesCount' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] ReportsVM -- 'reportsGetFilesList' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'reportsGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsMoveFile' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsRenameFile' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'reportsUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsUpdateTags' route
    :<|> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "File" :> ReqBody '[JSON] ReportCreateVM :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsUploadFile' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "groups" :> QueryParam "userId" Text :> Verb 'GET 200 '[JSON] GroupsVM -- 'subscriptionGroupsGetGroupsList' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> Capture "accessToken" Text :> "accept" :> Verb 'GET 200 '[JSON] () -- 'subscriptionInvitesAcceptInvite' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> ReqBody '[JSON] CreateSubscriptionInviteVM :> Verb 'POST 200 '[JSON] SubscriptionInviteVM -- 'subscriptionInvitesCreateInvite' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> Capture "accesstoken" Text :> Verb 'DELETE 200 '[JSON] () -- 'subscriptionInvitesDeleteInvite' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invites" :> Verb 'GET 200 '[JSON] SubscriptionInvitesVM -- 'subscriptionInvitesGetInvites' route
    :<|> "api" :> "manage" :> "v1" :> "SubscriptionPlans" :> Capture "id" Text :> Verb 'GET 200 '[JSON] SubscriptionPlanVM -- 'subscriptionPlansGetSubscriptionPlan' route
    :<|> "api" :> "manage" :> "v1" :> "SubscriptionPlans" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionPlansVM -- 'subscriptionPlansGetSubscriptionPlans' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> Capture "userId" Text :> Verb 'PUT 200 '[JSON] () -- 'subscriptionUsersAddUser' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionUsersVM -- 'subscriptionUsersGetUsers' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "leave" :> Verb 'DELETE 200 '[JSON] () -- 'subscriptionUsersLeaveSubscripiton' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> Capture "userId" Text :> Verb 'DELETE 200 '[JSON] () -- 'subscriptionUsersRemoveUser' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "defaultPermissions" :> Verb 'GET 200 '[JSON] DefaultPermissions -- 'subscriptionsGetDefaultPermissions' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subId" Text :> "mypermissions" :> Verb 'GET 200 '[JSON] MyPermissionsVM -- 'subscriptionsGetMyPermissions' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] SubscriptionPermissionsVM -- 'subscriptionsGetPermissions' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> Verb 'GET 200 '[JSON] SubscriptionVM -- 'subscriptionsGetSubscription' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionsVM -- 'subscriptionsGetSubscriptions' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "rename" :> ReqBody '[JSON] RenameSubscriptionVM :> Verb 'PUT 200 '[JSON] SubscriptionVM -- 'subscriptionsRenameSubscription' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "defaultPermissions" :> ReqBody '[JSON] UpdateDefaultPermissionsVM :> Verb 'PUT 200 '[JSON] DefaultPermissionsVM -- 'subscriptionsUpdateDefaultPermissions' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "Locale" :> ReqBody '[JSON] UpdateSubscriptionLocaleVM :> Verb 'PUT 200 '[JSON] SubscriptionVM -- 'subscriptionsUpdateLocale' route
    :<|> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateSubscriptionPermissionsVM :> Verb 'POST 200 '[JSON] () -- 'subscriptionsUpdatePermissions' route
    :<|> "api" :> "tasks" :> ReqBody '[JSON] CreateTaskBaseVM :> Verb 'POST 200 '[JSON] TaskBaseVM -- 'tasksCreateTask' route
    :<|> "api" :> "tasks" :> Capture "taskId" Text :> Verb 'DELETE 200 '[JSON] () -- 'tasksDeleteTask' route
    :<|> "api" :> "tasks" :> Capture "taskId" Text :> Verb 'GET 200 '[JSON] TaskBaseVM -- 'tasksGet' route
    :<|> "api" :> "tasks" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] TasksVM -- 'tasksGetList' route
    :<|> "api" :> "tasks" :> "run" :> ReqBody '[JSON] RunTaskBaseVM :> Verb 'POST 200 '[JSON] () -- 'tasksRunTask' route
    :<|> "api" :> "tasks" :> Capture "taskId" Text :> "run" :> Verb 'POST 200 '[JSON] () -- 'tasksRunTaskById' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'templateFolderAndFileGetCount' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> Verb 'GET 200 '[JSON] FilesVM -- 'templateFolderAndFileGetFoldersAndFiles' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersCopyFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> QueryParam "recursive" Bool :> Verb 'DELETE 200 '[JSON] () -- 'templateFoldersDeleteFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'templateFoldersGetBreadcrumbs' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'templateFoldersGetFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] FilesVM -- 'templateFoldersGetFolders' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'templateFoldersGetFoldersCount' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'templateFoldersGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'templateFoldersGetRootFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersMoveFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] TemplateFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersPostFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersRenameFolder' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'templateFoldersUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersUpdateTags' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesCopyFile' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] () -- 'templatesDeleteFile' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportTemplateVM :> Verb 'POST 200 '[JSON] ExportVM -- 'templatesExport' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TemplateVM -- 'templatesGetFile' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'templatesGetFilesCount' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] TemplatesVM -- 'templatesGetFilesList' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'templatesGetPermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesMoveFile' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Prepare" :> ReqBody '[JSON] PrepareTemplateVM :> Verb 'POST 200 '[JSON] ReportVM -- 'templatesPrepare' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesRenameFile' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesUpdateIcon' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 200 '[JSON] () -- 'templatesUpdatePermissions' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesUpdateTags' route
    :<|> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "File" :> ReqBody '[JSON] TemplateCreateVM :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesUploadFile' route
    :<|> "api" :> "manage" :> "v1" :> "UserProfile" :> Verb 'GET 200 '[JSON] UserProfileVM -- 'userProfileGetMyProfile' route
    :<|> "api" :> "manage" :> "v1" :> "UserProfile" :> Capture "userId" Text :> Verb 'GET 200 '[JSON] UserProfileVM -- 'userProfileGetUserProfile' route
    :<|> "api" :> "manage" :> "v1" :> "UserProfile" :> ReqBody '[JSON] UpdateUserProfileVM :> Verb 'PUT 200 '[JSON] () -- 'userProfileUpdateMyProfile' route
    :<|> "api" :> "manage" :> "v1" :> "UserSettings" :> Verb 'GET 200 '[JSON] UserSettingsVM -- 'userSettingsGetCurrentUserSettings' route
    :<|> "api" :> "manage" :> "v1" :> "UserSettings" :> ReqBody '[JSON] UpdateUserSettingsVM :> Verb 'PUT 200 '[JSON] UserSettingsVM -- 'userSettingsUpdateMySettings' route
    :<|> Raw 


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype FastReportCloudClientError = FastReportCloudClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for FastReportCloud.
-- The backend can be used both for the client and the server. The client generated from the FastReportCloud OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createFastReportCloudClient@). Alternatively, provided
-- a backend, the API can be served using @runFastReportCloudMiddlewareServer@.
data FastReportCloudBackend m = FastReportCloudBackend
  { apiKeysCreateApiKey :: CreateApiKeyVM -> m ApiKeyVM{- ^  -}
  , apiKeysDeleteApiKey :: DeleteApiKeyVM -> m (){- ^  -}
  , apiKeysGetApiKeys :: m ApiKeysVM{- ^ Always work, it should make only 200 response (except if user is not authorized). -}
  , configurationGet :: m ServerConfigurationVM{- ^  -}
  , dataSourcesCreateDataSource :: CreateDataSourceVM -> m DataSourceVM{- ^  -}
  , dataSourcesDeleteDataSource :: Text -> m (){- ^  -}
  , dataSourcesFetchData :: Text -> m (){- ^  -}
  , dataSourcesGetAvailableDataSources :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe DataSourceSorting -> Maybe Bool -> m DataSourcesVM{- ^  -}
  , dataSourcesGetDataSource :: Text -> m DataSourceVM{- ^  -}
  , dataSourcesGetPermissions :: Text -> m DataSourcePermissionsVM{- ^  -}
  , dataSourcesRenameDataSource :: Text -> RenameDataSourceVM -> m DataSourceVM{- ^  -}
  , dataSourcesUpdateConnectionString :: Text -> UpdateDataSourceConnectionStringVM -> m DataSourceVM{- ^  -}
  , dataSourcesUpdatePermissions :: Text -> UpdateDataSourcePermissionsVM -> m (){- ^  -}
  , dataSourcesUpdateSubscriptionDataSource :: Text -> UpdateDataSourceSubscriptionVM -> m (){- ^  -}
  , downloadGetExport :: Text -> m FilePath{- ^  -}
  , downloadGetExportThumbnail :: Text -> m FilePath{- ^  -}
  , downloadGetExports :: Text -> Maybe Text -> m FilePath{- ^  -}
  , downloadGetReport :: Text -> m FilePath{- ^  -}
  , downloadGetReportThumbnail :: Text -> m FilePath{- ^  -}
  , downloadGetReports :: Text -> Maybe Text -> m FilePath{- ^  -}
  , downloadGetTemplate :: Text -> m FilePath{- ^  -}
  , downloadGetTemplates :: Text -> Maybe Text -> m FilePath{- ^  -}
  , exportFolderAndFileGetCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , exportFolderAndFileGetFoldersAndFiles :: Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersCopyFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , exportFoldersDeleteFolder :: Text -> Maybe Bool -> m (){- ^ User with a Delete Entity permission can access this method. -}
  , exportFoldersGetBreadcrumbs :: Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFolder :: Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFolders :: Text -> Maybe Int -> Maybe Int -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFoldersCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , exportFoldersGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , exportFoldersGetRootFolder :: Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , exportFoldersMoveFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , exportFoldersPostFolder :: Text -> ExportFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , exportFoldersRenameFolder :: Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , exportFoldersUpdateIcon :: Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , exportFoldersUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , exportFoldersUpdateTags :: Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , exportsCopyFile :: Text -> Text -> m ExportVM{- ^  -}
  , exportsDeleteFile :: Text -> m (){- ^ User with Delete permission can access the method. -}
  , exportsGetFile :: Text -> m ExportVM{- ^ User with Get Entity permission can access this method. -}
  , exportsGetFilesCount :: Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , exportsGetFilesList :: Text -> Maybe Int -> Maybe Int -> m ExportsVM{- ^  -}
  , exportsGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , exportsMoveFile :: Text -> Text -> m ExportVM{- ^ User with Update Place permission can access this method. -}
  , exportsRenameFile :: Text -> FileRenameVM -> m ExportVM{- ^ User with Update Name permission can access this method. -}
  , exportsUpdateIcon :: Text -> FileIconVM -> m ExportVM{- ^ User with Update Icon permission can access this method. -}
  , exportsUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , exportsUpdateTags :: Text -> FileTagsUpdateVM -> m ExportVM{- ^ User with Update Tags permission can access this method. -}
  , groupUsersAddUserToGroup :: Text -> Text -> m (){- ^  -}
  , groupUsersGetUsersInGroup :: Text -> Maybe Int -> Maybe Int -> m GroupUsersVM{- ^  -}
  , groupUsersLeaveFromGroup :: Text -> m (){- ^  -}
  , groupUsersRemoveFromGroup :: Text -> Text -> m (){- ^  -}
  , groupsCreateGroup :: CreateGroupVM -> m GroupVM{- ^  -}
  , groupsDeleteGroup :: Text -> m (){- ^  -}
  , groupsGetGroup :: Text -> m GroupVM{- ^  -}
  , groupsGetGroupList :: Maybe Int -> Maybe Int -> m GroupsVM{- ^  -}
  , groupsGetPermissions :: Text -> m GroupPermissionsVM{- ^  -}
  , groupsRenameGroup :: Text -> RenameGroupVM -> m GroupVM{- ^  -}
  , groupsUpdatePermissions :: Text -> UpdateGroupPermissionsVM -> m (){- ^  -}
  , healthCheckDataGet :: m (){- ^  -}
  , reportFolderAndFileGetCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , reportFolderAndFileGetFoldersAndFiles :: Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersCopyFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , reportFoldersDeleteFolder :: Text -> Maybe Bool -> m (){- ^ User with a Delete Entity permission can access this method. -}
  , reportFoldersGetBreadcrumbs :: Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFolder :: Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFolders :: Text -> Maybe Int -> Maybe Int -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFoldersCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , reportFoldersGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , reportFoldersGetRootFolder :: Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , reportFoldersMoveFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , reportFoldersPostFolder :: Text -> ReportFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , reportFoldersRenameFolder :: Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , reportFoldersUpdateIcon :: Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , reportFoldersUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , reportFoldersUpdateTags :: Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , reportsCopyFile :: Text -> Text -> m ReportVM{- ^  -}
  , reportsDeleteFile :: Text -> m (){- ^ User with Delete permission can access the method. -}
  , reportsExport :: Text -> ExportReportVM -> m ExportVM{- ^ User with Execute Export permission on prepared report and  Create Entity on an export folder can access this method. -}
  , reportsGetFile :: Text -> m ReportVM{- ^ User with Get Entity permission can access this method. -}
  , reportsGetFilesCount :: Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , reportsGetFilesList :: Text -> Maybe Int -> Maybe Int -> m ReportsVM{- ^  -}
  , reportsGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , reportsMoveFile :: Text -> Text -> m ReportVM{- ^ User with Update Place permission can access this method. -}
  , reportsRenameFile :: Text -> FileRenameVM -> m ReportVM{- ^ User with Update Name permission can access this method. -}
  , reportsUpdateIcon :: Text -> FileIconVM -> m ReportVM{- ^ User with Update Icon permission can access this method. -}
  , reportsUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , reportsUpdateTags :: Text -> FileTagsUpdateVM -> m ReportVM{- ^ User with Update Tags permission can access this method. -}
  , reportsUploadFile :: Text -> ReportCreateVM -> m ReportVM{- ^ User with Create Entity permission can access this method. -}
  , subscriptionGroupsGetGroupsList :: Text -> Maybe Text -> m GroupsVM{- ^  -}
  , subscriptionInvitesAcceptInvite :: Text -> Text -> m (){- ^  -}
  , subscriptionInvitesCreateInvite :: Text -> CreateSubscriptionInviteVM -> m SubscriptionInviteVM{- ^  -}
  , subscriptionInvitesDeleteInvite :: Text -> Text -> m (){- ^  -}
  , subscriptionInvitesGetInvites :: Text -> m SubscriptionInvitesVM{- ^  -}
  , subscriptionPlansGetSubscriptionPlan :: Text -> m SubscriptionPlanVM{- ^  -}
  , subscriptionPlansGetSubscriptionPlans :: Maybe Int -> Maybe Int -> m SubscriptionPlansVM{- ^ If no active subscription plans, then the endpoint will return empty list -}
  , subscriptionUsersAddUser :: Text -> Text -> m (){- ^  -}
  , subscriptionUsersGetUsers :: Text -> Maybe Int -> Maybe Int -> m SubscriptionUsersVM{- ^  -}
  , subscriptionUsersLeaveSubscripiton :: Text -> m (){- ^  -}
  , subscriptionUsersRemoveUser :: Text -> Text -> m (){- ^  -}
  , subscriptionsGetDefaultPermissions :: Text -> m DefaultPermissions{- ^  -}
  , subscriptionsGetMyPermissions :: Text -> m MyPermissionsVM{- ^  -}
  , subscriptionsGetPermissions :: Text -> m SubscriptionPermissionsVM{- ^  -}
  , subscriptionsGetSubscription :: Text -> m SubscriptionVM{- ^  -}
  , subscriptionsGetSubscriptions :: Maybe Int -> Maybe Int -> m SubscriptionsVM{- ^  -}
  , subscriptionsRenameSubscription :: Text -> RenameSubscriptionVM -> m SubscriptionVM{- ^  -}
  , subscriptionsUpdateDefaultPermissions :: Text -> UpdateDefaultPermissionsVM -> m DefaultPermissionsVM{- ^  -}
  , subscriptionsUpdateLocale :: Text -> UpdateSubscriptionLocaleVM -> m SubscriptionVM{- ^  -}
  , subscriptionsUpdatePermissions :: Text -> UpdateSubscriptionPermissionsVM -> m (){- ^  -}
  , tasksCreateTask :: CreateTaskBaseVM -> m TaskBaseVM{- ^  -}
  , tasksDeleteTask :: Text -> m (){- ^  -}
  , tasksGet :: Text -> m TaskBaseVM{- ^  -}
  , tasksGetList :: Maybe Int -> Maybe Int -> Maybe Text -> m TasksVM{- ^  -}
  , tasksRunTask :: RunTaskBaseVM -> m (){- ^  -}
  , tasksRunTaskById :: Text -> m (){- ^  -}
  , templateFolderAndFileGetCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , templateFolderAndFileGetFoldersAndFiles :: Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersCopyFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , templateFoldersDeleteFolder :: Text -> Maybe Bool -> m (){- ^ User with a Delete Entity permission can access this method. -}
  , templateFoldersGetBreadcrumbs :: Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFolder :: Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFolders :: Text -> Maybe Int -> Maybe Int -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFoldersCount :: Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , templateFoldersGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , templateFoldersGetRootFolder :: Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , templateFoldersMoveFolder :: Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , templateFoldersPostFolder :: Text -> TemplateFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , templateFoldersRenameFolder :: Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , templateFoldersUpdateIcon :: Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , templateFoldersUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , templateFoldersUpdateTags :: Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , templatesCopyFile :: Text -> Text -> m TemplateVM{- ^  -}
  , templatesDeleteFile :: Text -> m (){- ^ User with Delete permission can access the method. -}
  , templatesExport :: Text -> ExportTemplateVM -> m ExportVM{- ^ User with Execute Export permission on prepared report and  Create Entity on an export folder can access this method. -}
  , templatesGetFile :: Text -> m TemplateVM{- ^ User with Get Entity permission can access this method. -}
  , templatesGetFilesCount :: Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , templatesGetFilesList :: Text -> Maybe Int -> Maybe Int -> m TemplatesVM{- ^  -}
  , templatesGetPermissions :: Text -> m FilePermissionsVM{- ^  -}
  , templatesMoveFile :: Text -> Text -> m TemplateVM{- ^ User with Update Place permission can access this method. -}
  , templatesPrepare :: Text -> PrepareTemplateVM -> m ReportVM{- ^ User with Execute Prepare permission on report and  Create Entity on a prepared report folder can access this method. -}
  , templatesRenameFile :: Text -> FileRenameVM -> m TemplateVM{- ^ User with Update Name permission can access this method. -}
  , templatesUpdateIcon :: Text -> FileIconVM -> m TemplateVM{- ^ User with Update Icon permission can access this method. -}
  , templatesUpdatePermissions :: Text -> UpdateFilePermissionsVM -> m (){- ^  -}
  , templatesUpdateTags :: Text -> FileTagsUpdateVM -> m TemplateVM{- ^ User with Update Tags permission can access this method. -}
  , templatesUploadFile :: Text -> TemplateCreateVM -> m TemplateVM{- ^ User with Create Entity permission can access this method. -}
  , userProfileGetMyProfile :: m UserProfileVM{- ^  -}
  , userProfileGetUserProfile :: Text -> m UserProfileVM{- ^  -}
  , userProfileUpdateMyProfile :: UpdateUserProfileVM -> m (){- ^ This method is only allowed for local sign in via intranet -}
  , userSettingsGetCurrentUserSettings :: m UserSettingsVM{- ^  -}
  , userSettingsUpdateMySettings :: UpdateUserSettingsVM -> m UserSettingsVM{- ^  -}
  }

newtype FastReportCloudClient a = FastReportCloudClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative FastReportCloudClient where
  pure x = FastReportCloudClient (\_ -> pure x)
  (FastReportCloudClient f) <*> (FastReportCloudClient x) =
    FastReportCloudClient (\env -> f env <*> x env)

instance Monad FastReportCloudClient where
  (FastReportCloudClient a) >>= f =
    FastReportCloudClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO FastReportCloudClient where
  liftIO io = FastReportCloudClient (\_ -> liftIO io)

createFastReportCloudClient :: FastReportCloudBackend FastReportCloudClient
createFastReportCloudClient = FastReportCloudBackend{..}
  where
    ((coerce -> apiKeysCreateApiKey) :<|>
     (coerce -> apiKeysDeleteApiKey) :<|>
     (coerce -> apiKeysGetApiKeys) :<|>
     (coerce -> configurationGet) :<|>
     (coerce -> dataSourcesCreateDataSource) :<|>
     (coerce -> dataSourcesDeleteDataSource) :<|>
     (coerce -> dataSourcesFetchData) :<|>
     (coerce -> dataSourcesGetAvailableDataSources) :<|>
     (coerce -> dataSourcesGetDataSource) :<|>
     (coerce -> dataSourcesGetPermissions) :<|>
     (coerce -> dataSourcesRenameDataSource) :<|>
     (coerce -> dataSourcesUpdateConnectionString) :<|>
     (coerce -> dataSourcesUpdatePermissions) :<|>
     (coerce -> dataSourcesUpdateSubscriptionDataSource) :<|>
     (coerce -> downloadGetExport) :<|>
     (coerce -> downloadGetExportThumbnail) :<|>
     (coerce -> downloadGetExports) :<|>
     (coerce -> downloadGetReport) :<|>
     (coerce -> downloadGetReportThumbnail) :<|>
     (coerce -> downloadGetReports) :<|>
     (coerce -> downloadGetTemplate) :<|>
     (coerce -> downloadGetTemplates) :<|>
     (coerce -> exportFolderAndFileGetCount) :<|>
     (coerce -> exportFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> exportFoldersCopyFolder) :<|>
     (coerce -> exportFoldersDeleteFolder) :<|>
     (coerce -> exportFoldersGetBreadcrumbs) :<|>
     (coerce -> exportFoldersGetFolder) :<|>
     (coerce -> exportFoldersGetFolders) :<|>
     (coerce -> exportFoldersGetFoldersCount) :<|>
     (coerce -> exportFoldersGetPermissions) :<|>
     (coerce -> exportFoldersGetRootFolder) :<|>
     (coerce -> exportFoldersMoveFolder) :<|>
     (coerce -> exportFoldersPostFolder) :<|>
     (coerce -> exportFoldersRenameFolder) :<|>
     (coerce -> exportFoldersUpdateIcon) :<|>
     (coerce -> exportFoldersUpdatePermissions) :<|>
     (coerce -> exportFoldersUpdateTags) :<|>
     (coerce -> exportsCopyFile) :<|>
     (coerce -> exportsDeleteFile) :<|>
     (coerce -> exportsGetFile) :<|>
     (coerce -> exportsGetFilesCount) :<|>
     (coerce -> exportsGetFilesList) :<|>
     (coerce -> exportsGetPermissions) :<|>
     (coerce -> exportsMoveFile) :<|>
     (coerce -> exportsRenameFile) :<|>
     (coerce -> exportsUpdateIcon) :<|>
     (coerce -> exportsUpdatePermissions) :<|>
     (coerce -> exportsUpdateTags) :<|>
     (coerce -> groupUsersAddUserToGroup) :<|>
     (coerce -> groupUsersGetUsersInGroup) :<|>
     (coerce -> groupUsersLeaveFromGroup) :<|>
     (coerce -> groupUsersRemoveFromGroup) :<|>
     (coerce -> groupsCreateGroup) :<|>
     (coerce -> groupsDeleteGroup) :<|>
     (coerce -> groupsGetGroup) :<|>
     (coerce -> groupsGetGroupList) :<|>
     (coerce -> groupsGetPermissions) :<|>
     (coerce -> groupsRenameGroup) :<|>
     (coerce -> groupsUpdatePermissions) :<|>
     (coerce -> healthCheckDataGet) :<|>
     (coerce -> reportFolderAndFileGetCount) :<|>
     (coerce -> reportFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> reportFoldersCopyFolder) :<|>
     (coerce -> reportFoldersDeleteFolder) :<|>
     (coerce -> reportFoldersGetBreadcrumbs) :<|>
     (coerce -> reportFoldersGetFolder) :<|>
     (coerce -> reportFoldersGetFolders) :<|>
     (coerce -> reportFoldersGetFoldersCount) :<|>
     (coerce -> reportFoldersGetPermissions) :<|>
     (coerce -> reportFoldersGetRootFolder) :<|>
     (coerce -> reportFoldersMoveFolder) :<|>
     (coerce -> reportFoldersPostFolder) :<|>
     (coerce -> reportFoldersRenameFolder) :<|>
     (coerce -> reportFoldersUpdateIcon) :<|>
     (coerce -> reportFoldersUpdatePermissions) :<|>
     (coerce -> reportFoldersUpdateTags) :<|>
     (coerce -> reportsCopyFile) :<|>
     (coerce -> reportsDeleteFile) :<|>
     (coerce -> reportsExport) :<|>
     (coerce -> reportsGetFile) :<|>
     (coerce -> reportsGetFilesCount) :<|>
     (coerce -> reportsGetFilesList) :<|>
     (coerce -> reportsGetPermissions) :<|>
     (coerce -> reportsMoveFile) :<|>
     (coerce -> reportsRenameFile) :<|>
     (coerce -> reportsUpdateIcon) :<|>
     (coerce -> reportsUpdatePermissions) :<|>
     (coerce -> reportsUpdateTags) :<|>
     (coerce -> reportsUploadFile) :<|>
     (coerce -> subscriptionGroupsGetGroupsList) :<|>
     (coerce -> subscriptionInvitesAcceptInvite) :<|>
     (coerce -> subscriptionInvitesCreateInvite) :<|>
     (coerce -> subscriptionInvitesDeleteInvite) :<|>
     (coerce -> subscriptionInvitesGetInvites) :<|>
     (coerce -> subscriptionPlansGetSubscriptionPlan) :<|>
     (coerce -> subscriptionPlansGetSubscriptionPlans) :<|>
     (coerce -> subscriptionUsersAddUser) :<|>
     (coerce -> subscriptionUsersGetUsers) :<|>
     (coerce -> subscriptionUsersLeaveSubscripiton) :<|>
     (coerce -> subscriptionUsersRemoveUser) :<|>
     (coerce -> subscriptionsGetDefaultPermissions) :<|>
     (coerce -> subscriptionsGetMyPermissions) :<|>
     (coerce -> subscriptionsGetPermissions) :<|>
     (coerce -> subscriptionsGetSubscription) :<|>
     (coerce -> subscriptionsGetSubscriptions) :<|>
     (coerce -> subscriptionsRenameSubscription) :<|>
     (coerce -> subscriptionsUpdateDefaultPermissions) :<|>
     (coerce -> subscriptionsUpdateLocale) :<|>
     (coerce -> subscriptionsUpdatePermissions) :<|>
     (coerce -> tasksCreateTask) :<|>
     (coerce -> tasksDeleteTask) :<|>
     (coerce -> tasksGet) :<|>
     (coerce -> tasksGetList) :<|>
     (coerce -> tasksRunTask) :<|>
     (coerce -> tasksRunTaskById) :<|>
     (coerce -> templateFolderAndFileGetCount) :<|>
     (coerce -> templateFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> templateFoldersCopyFolder) :<|>
     (coerce -> templateFoldersDeleteFolder) :<|>
     (coerce -> templateFoldersGetBreadcrumbs) :<|>
     (coerce -> templateFoldersGetFolder) :<|>
     (coerce -> templateFoldersGetFolders) :<|>
     (coerce -> templateFoldersGetFoldersCount) :<|>
     (coerce -> templateFoldersGetPermissions) :<|>
     (coerce -> templateFoldersGetRootFolder) :<|>
     (coerce -> templateFoldersMoveFolder) :<|>
     (coerce -> templateFoldersPostFolder) :<|>
     (coerce -> templateFoldersRenameFolder) :<|>
     (coerce -> templateFoldersUpdateIcon) :<|>
     (coerce -> templateFoldersUpdatePermissions) :<|>
     (coerce -> templateFoldersUpdateTags) :<|>
     (coerce -> templatesCopyFile) :<|>
     (coerce -> templatesDeleteFile) :<|>
     (coerce -> templatesExport) :<|>
     (coerce -> templatesGetFile) :<|>
     (coerce -> templatesGetFilesCount) :<|>
     (coerce -> templatesGetFilesList) :<|>
     (coerce -> templatesGetPermissions) :<|>
     (coerce -> templatesMoveFile) :<|>
     (coerce -> templatesPrepare) :<|>
     (coerce -> templatesRenameFile) :<|>
     (coerce -> templatesUpdateIcon) :<|>
     (coerce -> templatesUpdatePermissions) :<|>
     (coerce -> templatesUpdateTags) :<|>
     (coerce -> templatesUploadFile) :<|>
     (coerce -> userProfileGetMyProfile) :<|>
     (coerce -> userProfileGetUserProfile) :<|>
     (coerce -> userProfileUpdateMyProfile) :<|>
     (coerce -> userSettingsGetCurrentUserSettings) :<|>
     (coerce -> userSettingsUpdateMySettings) :<|>
     _) = client (Proxy :: Proxy FastReportCloudAPI)

-- | Run requests in the FastReportCloudClient monad.
runFastReportCloudClient :: Config -> FastReportCloudClient a -> ExceptT ClientError IO a
runFastReportCloudClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runFastReportCloudClientWithManager manager clientConfig cl

-- | Run requests in the FastReportCloudClient monad using a custom manager.
runFastReportCloudClientWithManager :: Manager -> Config -> FastReportCloudClient a -> ExceptT ClientError IO a
runFastReportCloudClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a FastReportCloudClientError
callFastReportCloud
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> FastReportCloudClient a -> m a
callFastReportCloud env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (FastReportCloudClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the FastReportCloud server at the provided host and port.
runFastReportCloudServer
  :: (MonadIO m, MonadThrow m)
  => Config -> FastReportCloudBackend (ExceptT ServerError IO) -> m ()
runFastReportCloudServer config backend = runFastReportCloudMiddlewareServer config requestMiddlewareId backend

-- | Run the FastReportCloud server at the provided host and port.
runFastReportCloudMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> FastReportCloudBackend (ExceptT ServerError IO) -> m ()
runFastReportCloudMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serve (Proxy :: Proxy FastReportCloudAPI) (serverFromBackend backend)
  where
    serverFromBackend FastReportCloudBackend{..} =
      (coerce apiKeysCreateApiKey :<|>
       coerce apiKeysDeleteApiKey :<|>
       coerce apiKeysGetApiKeys :<|>
       coerce configurationGet :<|>
       coerce dataSourcesCreateDataSource :<|>
       coerce dataSourcesDeleteDataSource :<|>
       coerce dataSourcesFetchData :<|>
       coerce dataSourcesGetAvailableDataSources :<|>
       coerce dataSourcesGetDataSource :<|>
       coerce dataSourcesGetPermissions :<|>
       coerce dataSourcesRenameDataSource :<|>
       coerce dataSourcesUpdateConnectionString :<|>
       coerce dataSourcesUpdatePermissions :<|>
       coerce dataSourcesUpdateSubscriptionDataSource :<|>
       coerce downloadGetExport :<|>
       coerce downloadGetExportThumbnail :<|>
       coerce downloadGetExports :<|>
       coerce downloadGetReport :<|>
       coerce downloadGetReportThumbnail :<|>
       coerce downloadGetReports :<|>
       coerce downloadGetTemplate :<|>
       coerce downloadGetTemplates :<|>
       coerce exportFolderAndFileGetCount :<|>
       coerce exportFolderAndFileGetFoldersAndFiles :<|>
       coerce exportFoldersCopyFolder :<|>
       coerce exportFoldersDeleteFolder :<|>
       coerce exportFoldersGetBreadcrumbs :<|>
       coerce exportFoldersGetFolder :<|>
       coerce exportFoldersGetFolders :<|>
       coerce exportFoldersGetFoldersCount :<|>
       coerce exportFoldersGetPermissions :<|>
       coerce exportFoldersGetRootFolder :<|>
       coerce exportFoldersMoveFolder :<|>
       coerce exportFoldersPostFolder :<|>
       coerce exportFoldersRenameFolder :<|>
       coerce exportFoldersUpdateIcon :<|>
       coerce exportFoldersUpdatePermissions :<|>
       coerce exportFoldersUpdateTags :<|>
       coerce exportsCopyFile :<|>
       coerce exportsDeleteFile :<|>
       coerce exportsGetFile :<|>
       coerce exportsGetFilesCount :<|>
       coerce exportsGetFilesList :<|>
       coerce exportsGetPermissions :<|>
       coerce exportsMoveFile :<|>
       coerce exportsRenameFile :<|>
       coerce exportsUpdateIcon :<|>
       coerce exportsUpdatePermissions :<|>
       coerce exportsUpdateTags :<|>
       coerce groupUsersAddUserToGroup :<|>
       coerce groupUsersGetUsersInGroup :<|>
       coerce groupUsersLeaveFromGroup :<|>
       coerce groupUsersRemoveFromGroup :<|>
       coerce groupsCreateGroup :<|>
       coerce groupsDeleteGroup :<|>
       coerce groupsGetGroup :<|>
       coerce groupsGetGroupList :<|>
       coerce groupsGetPermissions :<|>
       coerce groupsRenameGroup :<|>
       coerce groupsUpdatePermissions :<|>
       coerce healthCheckDataGet :<|>
       coerce reportFolderAndFileGetCount :<|>
       coerce reportFolderAndFileGetFoldersAndFiles :<|>
       coerce reportFoldersCopyFolder :<|>
       coerce reportFoldersDeleteFolder :<|>
       coerce reportFoldersGetBreadcrumbs :<|>
       coerce reportFoldersGetFolder :<|>
       coerce reportFoldersGetFolders :<|>
       coerce reportFoldersGetFoldersCount :<|>
       coerce reportFoldersGetPermissions :<|>
       coerce reportFoldersGetRootFolder :<|>
       coerce reportFoldersMoveFolder :<|>
       coerce reportFoldersPostFolder :<|>
       coerce reportFoldersRenameFolder :<|>
       coerce reportFoldersUpdateIcon :<|>
       coerce reportFoldersUpdatePermissions :<|>
       coerce reportFoldersUpdateTags :<|>
       coerce reportsCopyFile :<|>
       coerce reportsDeleteFile :<|>
       coerce reportsExport :<|>
       coerce reportsGetFile :<|>
       coerce reportsGetFilesCount :<|>
       coerce reportsGetFilesList :<|>
       coerce reportsGetPermissions :<|>
       coerce reportsMoveFile :<|>
       coerce reportsRenameFile :<|>
       coerce reportsUpdateIcon :<|>
       coerce reportsUpdatePermissions :<|>
       coerce reportsUpdateTags :<|>
       coerce reportsUploadFile :<|>
       coerce subscriptionGroupsGetGroupsList :<|>
       coerce subscriptionInvitesAcceptInvite :<|>
       coerce subscriptionInvitesCreateInvite :<|>
       coerce subscriptionInvitesDeleteInvite :<|>
       coerce subscriptionInvitesGetInvites :<|>
       coerce subscriptionPlansGetSubscriptionPlan :<|>
       coerce subscriptionPlansGetSubscriptionPlans :<|>
       coerce subscriptionUsersAddUser :<|>
       coerce subscriptionUsersGetUsers :<|>
       coerce subscriptionUsersLeaveSubscripiton :<|>
       coerce subscriptionUsersRemoveUser :<|>
       coerce subscriptionsGetDefaultPermissions :<|>
       coerce subscriptionsGetMyPermissions :<|>
       coerce subscriptionsGetPermissions :<|>
       coerce subscriptionsGetSubscription :<|>
       coerce subscriptionsGetSubscriptions :<|>
       coerce subscriptionsRenameSubscription :<|>
       coerce subscriptionsUpdateDefaultPermissions :<|>
       coerce subscriptionsUpdateLocale :<|>
       coerce subscriptionsUpdatePermissions :<|>
       coerce tasksCreateTask :<|>
       coerce tasksDeleteTask :<|>
       coerce tasksGet :<|>
       coerce tasksGetList :<|>
       coerce tasksRunTask :<|>
       coerce tasksRunTaskById :<|>
       coerce templateFolderAndFileGetCount :<|>
       coerce templateFolderAndFileGetFoldersAndFiles :<|>
       coerce templateFoldersCopyFolder :<|>
       coerce templateFoldersDeleteFolder :<|>
       coerce templateFoldersGetBreadcrumbs :<|>
       coerce templateFoldersGetFolder :<|>
       coerce templateFoldersGetFolders :<|>
       coerce templateFoldersGetFoldersCount :<|>
       coerce templateFoldersGetPermissions :<|>
       coerce templateFoldersGetRootFolder :<|>
       coerce templateFoldersMoveFolder :<|>
       coerce templateFoldersPostFolder :<|>
       coerce templateFoldersRenameFolder :<|>
       coerce templateFoldersUpdateIcon :<|>
       coerce templateFoldersUpdatePermissions :<|>
       coerce templateFoldersUpdateTags :<|>
       coerce templatesCopyFile :<|>
       coerce templatesDeleteFile :<|>
       coerce templatesExport :<|>
       coerce templatesGetFile :<|>
       coerce templatesGetFilesCount :<|>
       coerce templatesGetFilesList :<|>
       coerce templatesGetPermissions :<|>
       coerce templatesMoveFile :<|>
       coerce templatesPrepare :<|>
       coerce templatesRenameFile :<|>
       coerce templatesUpdateIcon :<|>
       coerce templatesUpdatePermissions :<|>
       coerce templatesUpdateTags :<|>
       coerce templatesUploadFile :<|>
       coerce userProfileGetMyProfile :<|>
       coerce userProfileGetUserProfile :<|>
       coerce userProfileUpdateMyProfile :<|>
       coerce userSettingsGetCurrentUserSettings :<|>
       coerce userSettingsUpdateMySettings :<|>
       serveDirectoryFileServer "static")
