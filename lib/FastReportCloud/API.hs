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
  ( -- * Client and Server
    Config(..)
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
  -- ** Plain WAI Application
  , serverWaiApplicationFastReportCloud
  -- ** Authentication
  , FastReportCloudAuth(..)
  , clientAuth
  , Protected
  ) where

import           FastReportCloud.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
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
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.HttpAuth    (extractBearerAuth)
import           Network.Wai.Middleware.HttpAuth    (extractBasicAuth)
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.BasicAuth              (BasicAuthData (..))
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest, basicAuthReq, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
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
    =    Protected :> "api" :> "manage" :> "v1" :> "ApiKeys" :> ReqBody '[JSON] CreateApiKeyVM :> Verb 'POST 200 '[JSON] ApiKeyVM -- 'apiKeysCreateApiKey' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "ApiKeys" :> ReqBody '[JSON] DeleteApiKeyVM :> Verb 'DELETE 204 '[JSON] NoContent -- 'apiKeysDeleteApiKey' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "ApiKeys" :> Verb 'GET 200 '[JSON] ApiKeysVM -- 'apiKeysGetApiKeys' route
    :<|> Protected :> "api" :> "v1" :> "Configuration" :> Verb 'GET 200 '[JSON] ServerConfigurationVM -- 'configurationGet' route
    :<|> Protected :> "api" :> "v1" :> "ContactGroups" :> "group" :> ReqBody '[JSON] CreateContactGroupVM :> Verb 'POST 200 '[JSON] ContactGroupVM -- 'contactGroupsCreate' route
    :<|> Protected :> "api" :> "v1" :> "ContactGroups" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'contactGroupsDelete' route
    :<|> Protected :> "api" :> "v1" :> "ContactGroups" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ContactGroupVM -- 'contactGroupsGet' route
    :<|> Protected :> "api" :> "v1" :> "ContactGroups" :> "subscription" :> Capture "subscriptionId" Text :> "groups" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] ContactGroupsVM -- 'contactGroupsGetList' route
    :<|> Protected :> "api" :> "v1" :> "ContactGroups" :> Capture "id" Text :> ReqBody '[JSON] UpdateContactGroupVM :> Verb 'PUT 200 '[JSON] ContactGroupVM -- 'contactGroupsUpdate' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> "contact" :> ReqBody '[JSON] CreateContactVM :> Verb 'POST 200 '[JSON] ContactVM -- 'contactsCreate' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'contactsDelete' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ContactVM -- 'contactsGet' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> "group" :> Capture "groupId" Text :> "contacts" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] ContactsVM -- 'contactsGetByGroup' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> "subscription" :> Capture "subscriptionId" Text :> "contacts" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "searchPattern" Text :> Verb 'GET 200 '[JSON] ContactsVM -- 'contactsGetList' route
    :<|> Protected :> "api" :> "v1" :> "Contacts" :> Capture "id" Text :> ReqBody '[JSON] UpdateContactVM :> Verb 'PUT 200 '[JSON] ContactVM -- 'contactsUpdate' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "subscriptionId" Text :> "count" :> Verb 'GET 200 '[JSON] Integer -- 'dataSourcesCountDataSourcesAsync' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> ReqBody '[JSON] CreateDataSourceVM :> Verb 'POST 200 '[JSON] DataSourceVM -- 'dataSourcesCreateDataSource' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'dataSourcesDeleteDataSource' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "fetch" :> Verb 'GET 204 '[JSON] NoContent -- 'dataSourcesFetchData' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> QueryParam "subscriptionId" Text :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" DataSourceSorting :> QueryParam "desc" Bool :> Verb 'GET 200 '[JSON] DataSourcesVM -- 'dataSourcesGetAvailableDataSources' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> Verb 'GET 200 '[JSON] DataSourceVM -- 'dataSourcesGetDataSource' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] DataSourcePermissionsVM -- 'dataSourcesGetPermissions' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "rename" :> ReqBody '[JSON] RenameDataSourceVM :> Verb 'PUT 200 '[JSON] DataSourceVM -- 'dataSourcesRenameDataSource' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "ConnectionString" :> ReqBody '[JSON] UpdateDataSourceConnectionStringVM :> Verb 'PUT 200 '[JSON] DataSourceVM -- 'dataSourcesUpdateConnectionString' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateDataSourcePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'dataSourcesUpdatePermissions' route
    :<|> Protected :> "api" :> "data" :> "v1" :> "DataSources" :> Capture "id" Text :> "updateSubscription" :> ReqBody '[JSON] UpdateDataSourceSubscriptionVM :> Verb 'PUT 200 '[JSON] NoContent -- 'dataSourcesUpdateSubscriptionDataSource' route
    :<|> Protected :> "download" :> "e" :> Capture "id" Text :> QueryParam "preview" Bool :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExport' route
    :<|> Protected :> "download" :> "e" :> Capture "id" Text :> "thumbnail" :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExportThumbnail' route
    :<|> Protected :> "download" :> "es" :> Capture "archiveName" Text :> QueryParam "fileIds" Text :> QueryParam "folderIds" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetExports' route
    :<|> Protected :> "download" :> "lastPreview" :> Capture "reportId" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetLastSVGExport' route
    :<|> Protected :> "download" :> "r" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReport' route
    :<|> Protected :> "download" :> "r" :> Capture "id" Text :> "thumbnail" :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReportThumbnail' route
    :<|> Protected :> "download" :> "rs" :> Capture "archiveName" Text :> QueryParam "fileIds" Text :> QueryParam "folderIds" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetReports' route
    :<|> Protected :> "download" :> "t" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetTemplate' route
    :<|> Protected :> "download" :> "t" :> Capture "id" Text :> "thumbnail" :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetTemplateThumbnail' route
    :<|> Protected :> "download" :> "ts" :> Capture "archiveName" Text :> QueryParam "fileIds" Text :> QueryParam "folderIds" Text :> Verb 'GET 200 '[JSON] FilePath -- 'downloadGetTemplates' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> Capture "subscriptionId" Text :> "ClearRecycleBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'exportFolderAndFileClearRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> Capture "subscriptionId" Text :> "DeleteFiles" :> ReqBody '[JSON] SelectedFilesForDeletingVM :> Verb 'POST 204 '[JSON] NoContent -- 'exportFolderAndFileDeleteFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] CountVM -- 'exportFolderAndFileGetCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'exportFolderAndFileGetFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> Capture "subscriptionId" Text :> "ListRecycleBinFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'exportFolderAndFileGetRecycleBinFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> Capture "subscriptionId" Text :> "RecoverRecycleBin" :> Verb 'POST 204 '[JSON] NoContent -- 'exportFolderAndFileRecoverAllFromRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "size" :> Verb 'GET 200 '[JSON] FolderSizeVM -- 'exportFoldersCalculateFolderSize' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersCopyFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'exportFoldersDeleteFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'exportFoldersGetBreadcrumbs' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'exportFoldersGetFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'exportFoldersGetFolders' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'exportFoldersGetFoldersCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> "getOrCreate" :> QueryParam "name" Text :> QueryParam "subscriptionId" Text :> QueryParam "parentId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'exportFoldersGetOrCreate' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'exportFoldersGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'exportFoldersGetRootFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersMoveFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'exportFoldersMoveFolderToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] ExportFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'exportFoldersPostFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'exportFoldersRecoverFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersRenameFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'exportFoldersUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'exportFoldersUpdateTags' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ExportVM -- 'exportsCopyFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'exportsDeleteFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ExportVM -- 'exportsGetFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "History" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] AuditActionsVM -- 'exportsGetFileHistory' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'exportsGetFilesCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "searchPattern" Text :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] ExportsVM -- 'exportsGetFilesList' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'exportsGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ExportVM -- 'exportsMoveFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'exportsMoveFileToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'exportsRecoverFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsRenameFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'exportsUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Exports" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] ExportVM -- 'exportsUpdateTags' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> Capture "userId" Text :> Verb 'PUT 204 '[JSON] NoContent -- 'groupUsersAddUserToGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] GroupUsersVM -- 'groupUsersGetUsersInGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "leave" :> Verb 'DELETE 204 '[JSON] NoContent -- 'groupUsersLeaveFromGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "Users" :> Capture "userId" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'groupUsersRemoveFromGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> ReqBody '[JSON] CreateGroupVM :> Verb 'POST 200 '[JSON] GroupVM -- 'groupsCreateGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'groupsDeleteGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> Verb 'GET 200 '[JSON] GroupVM -- 'groupsGetGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] GroupsVM -- 'groupsGetGroupList' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] GroupPermissionsVM -- 'groupsGetPermissions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "rename" :> ReqBody '[JSON] RenameGroupVM :> Verb 'PUT 200 '[JSON] GroupVM -- 'groupsRenameGroup' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Groups" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateGroupPermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'groupsUpdatePermissions' route
    :<|> Protected :> "api" :> "backend" :> "v1" :> "HealthCheck" :> Verb 'GET 200 '[JSON] NoContent -- 'healthCheckDataGet' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> Capture "subscriptionId" Text :> "ClearRecycleBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'reportFolderAndFileClearRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> Capture "subscriptionId" Text :> "DeleteFiles" :> ReqBody '[JSON] SelectedFilesForDeletingVM :> Verb 'POST 204 '[JSON] NoContent -- 'reportFolderAndFileDeleteFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] CountVM -- 'reportFolderAndFileGetCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'reportFolderAndFileGetFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> Capture "subscriptionId" Text :> "ListRecycleBinFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'reportFolderAndFileGetRecycleBinFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> Capture "subscriptionId" Text :> "RecoverRecycleBin" :> Verb 'POST 204 '[JSON] NoContent -- 'reportFolderAndFileRecoverAllFromRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "size" :> Verb 'GET 200 '[JSON] FolderSizeVM -- 'reportFoldersCalculateFolderSize' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersCopyFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'reportFoldersDeleteFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportReportVM :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersExport' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'reportFoldersGetBreadcrumbs' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'reportFoldersGetFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'reportFoldersGetFolders' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'reportFoldersGetFoldersCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> "getOrCreate" :> QueryParam "name" Text :> QueryParam "subscriptionId" Text :> QueryParam "parentId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'reportFoldersGetOrCreate' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'reportFoldersGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'reportFoldersGetRootFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersMoveFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'reportFoldersMoveFolderToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] ReportFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'reportFoldersPostFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'reportFoldersRecoverFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersRenameFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'reportFoldersUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'reportFoldersUpdateTags' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsCopyFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'reportsDeleteFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportReportVM :> Verb 'POST 200 '[JSON] ExportVM -- 'reportsExport' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] ReportVM -- 'reportsGetFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "History" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] AuditActionsVM -- 'reportsGetFileHistory' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'reportsGetFilesCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "searchPattern" Text :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] ReportsVM -- 'reportsGetFilesList' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'reportsGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsMoveFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'reportsMoveFileToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'reportsRecoverFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsRenameFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "StaticPreview" :> ReqBody '[JSON] PreviewReportVM :> Verb 'POST 200 '[JSON] ExportVM -- 'reportsStaticPreview' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'reportsUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] ReportVM -- 'reportsUpdateTags' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Reports" :> "Folder" :> Capture "id" Text :> "File" :> ReqBody '[JSON] ReportCreateVM :> Verb 'POST 200 '[JSON] ReportVM -- 'reportsUploadFile' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "count" :> Verb 'GET 200 '[JSON] Integer -- 'subscriptionGroupsCountGroupsAsync' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "groups" :> QueryParam "userId" Text :> Verb 'GET 200 '[JSON] GroupsVM -- 'subscriptionGroupsGetGroupsList' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> Capture "accessToken" Text :> "accept" :> Verb 'GET 200 '[JSON] NoContent -- 'subscriptionInvitesAcceptInvite' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> ReqBody '[JSON] CreateSubscriptionInviteVM :> Verb 'POST 200 '[JSON] SubscriptionInviteVM -- 'subscriptionInvitesCreateInvite' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invite" :> Capture "accesstoken" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'subscriptionInvitesDeleteInvite' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "invites" :> Verb 'GET 200 '[JSON] SubscriptionInvitesVM -- 'subscriptionInvitesGetInvites' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "SubscriptionPlans" :> Capture "id" Text :> Verb 'GET 200 '[JSON] SubscriptionPlanVM -- 'subscriptionPlansGetSubscriptionPlan' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "SubscriptionPlans" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionPlansVM -- 'subscriptionPlansGetSubscriptionPlans' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> Capture "userId" Text :> Verb 'PUT 204 '[JSON] NoContent -- 'subscriptionUsersAddUser' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "UsersCount" :> Verb 'GET 200 '[JSON] Integer -- 'subscriptionUsersCountUsersAsync' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionUsersVM -- 'subscriptionUsersGetUsers' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "leave" :> Verb 'DELETE 204 '[JSON] NoContent -- 'subscriptionUsersLeaveSubscripiton' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "users" :> Capture "userId" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'subscriptionUsersRemoveUser' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "defaultPermissions" :> Verb 'GET 200 '[JSON] DefaultPermissionsVM -- 'subscriptionsGetDefaultPermissions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subId" Text :> "mypermissions" :> Verb 'GET 200 '[JSON] MyPermissionsVM -- 'subscriptionsGetMyPermissions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] SubscriptionPermissionsVM -- 'subscriptionsGetPermissions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> Verb 'GET 200 '[JSON] SubscriptionVM -- 'subscriptionsGetSubscription' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] SubscriptionsVM -- 'subscriptionsGetSubscriptions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "rename" :> ReqBody '[JSON] RenameSubscriptionVM :> Verb 'PUT 200 '[JSON] SubscriptionVM -- 'subscriptionsRenameSubscription' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "defaultPermissions" :> ReqBody '[JSON] UpdateDefaultPermissionsVM :> Verb 'PUT 200 '[JSON] DefaultPermissionsVM -- 'subscriptionsUpdateDefaultPermissions' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "subscriptionId" Text :> "Locale" :> ReqBody '[JSON] UpdateSubscriptionLocaleVM :> Verb 'PUT 200 '[JSON] SubscriptionVM -- 'subscriptionsUpdateLocale' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "Subscriptions" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateSubscriptionPermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'subscriptionsUpdatePermissions' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> ReqBody '[JSON] CreateTaskBaseVM :> Verb 'POST 200 '[JSON] TaskBaseVM -- 'tasksCreateTask' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "taskId" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'tasksDeleteTask' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "taskId" Text :> Verb 'GET 200 '[JSON] TaskBaseVM -- 'tasksGet' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "subscriptionId" Text :> QueryParam "searchPattern" Text :> Verb 'GET 200 '[JSON] TasksVM -- 'tasksGetList' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] TaskPermissionsVM -- 'tasksGetPermissions' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "taskId" Text :> "rename" :> QueryParam "newName" Text :> Verb 'PUT 200 '[JSON] TaskBaseVM -- 'tasksRenameTask' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> "run" :> ReqBody '[JSON] RunTaskBaseVM :> Verb 'POST 200 '[JSON] NoContent -- 'tasksRunTask' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "taskId" Text :> "run" :> Verb 'POST 200 '[JSON] NoContent -- 'tasksRunTaskById' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateTaskPermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'tasksUpdatePermissions' route
    :<|> Protected :> "api" :> "tasks" :> "v1" :> "Tasks" :> Capture "taskId" Text :> ReqBody '[JSON] UpdateTaskBaseVM :> Verb 'PUT 200 '[JSON] TaskBaseVM -- 'tasksUpdateTask' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> Capture "subscriptionId" Text :> "ClearRecycleBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'templateFolderAndFileClearRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> Capture "subscriptionId" Text :> "DeleteFiles" :> ReqBody '[JSON] SelectedFilesForDeletingVM :> Verb 'POST 204 '[JSON] NoContent -- 'templateFolderAndFileDeleteFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFolderAndFiles" :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] CountVM -- 'templateFolderAndFileGetCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'templateFolderAndFileGetFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> Capture "subscriptionId" Text :> "ListRecycleBinFolderAndFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'templateFolderAndFileGetRecycleBinFoldersAndFiles' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> Capture "subscriptionId" Text :> "RecoverRecycleBin" :> Verb 'POST 204 '[JSON] NoContent -- 'templateFolderAndFileRecoverAllFromRecycleBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "size" :> Verb 'GET 200 '[JSON] FolderSizeVM -- 'templateFoldersCalculateFolderSize' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersCopyFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'templateFoldersDeleteFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportTemplateVM :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersExport' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Breadcrumbs" :> Verb 'GET 200 '[JSON] BreadcrumbsVM -- 'templateFoldersGetBreadcrumbs' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> Verb 'GET 200 '[JSON] FileVM -- 'templateFoldersGetFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFolders" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "searchPattern" Text :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] FilesVM -- 'templateFoldersGetFolders' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFolders" :> Verb 'GET 200 '[JSON] CountVM -- 'templateFoldersGetFoldersCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> "getOrCreate" :> QueryParam "name" Text :> QueryParam "subscriptionId" Text :> QueryParam "parentId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'templateFoldersGetOrCreate' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'templateFoldersGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Root" :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] FileVM -- 'templateFoldersGetRootFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersMoveFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'templateFoldersMoveFolderToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Folder" :> ReqBody '[JSON] TemplateFolderCreateVM :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersPostFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Prepare" :> ReqBody '[JSON] PrepareTemplateVM :> Verb 'POST 200 '[JSON] FileVM -- 'templateFoldersPrepare' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'templateFoldersRecoverFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FolderRenameVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersRenameFolder' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FolderIconVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'templateFoldersUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FolderTagsUpdateVM :> Verb 'PUT 200 '[JSON] FileVM -- 'templateFoldersUpdateTags' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Copy" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesCopyFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> Verb 'DELETE 204 '[JSON] NoContent -- 'templatesDeleteFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Export" :> ReqBody '[JSON] ExportTemplateVM :> Verb 'POST 200 '[JSON] ExportVM -- 'templatesExport' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TemplateVM -- 'templatesGetFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "History" :> QueryParam "skip" Int :> QueryParam "take" Int :> Verb 'GET 200 '[JSON] AuditActionsVM -- 'templatesGetFileHistory' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "CountFiles" :> Verb 'GET 200 '[JSON] CountVM -- 'templatesGetFilesCount' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "ListFiles" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "searchPattern" Text :> QueryParam "orderBy" FileSorting :> QueryParam "desc" Bool :> QueryParam "useRegex" Bool :> Verb 'GET 200 '[JSON] TemplatesVM -- 'templatesGetFilesList' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "permissions" :> Verb 'GET 200 '[JSON] FilePermissionsVM -- 'templatesGetPermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Move" :> Capture "folderId" Text :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesMoveFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "ToBin" :> Verb 'DELETE 204 '[JSON] NoContent -- 'templatesMoveFileToBin' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Prepare" :> ReqBody '[JSON] PrepareTemplateVM :> Verb 'POST 200 '[JSON] ReportVM -- 'templatesPrepare' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Recover" :> QueryParam "recoveryPath" Text :> Verb 'POST 204 '[JSON] NoContent -- 'templatesRecoverFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Rename" :> ReqBody '[JSON] FileRenameVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesRenameFile' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "StaticPreview" :> ReqBody '[JSON] PreviewTemplateVM :> Verb 'POST 200 '[JSON] ExportVM -- 'templatesStaticPreview' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Content" :> ReqBody '[JSON] UpdateFileContentVM :> Verb 'PUT 204 '[JSON] NoContent -- 'templatesUpdateContent' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "Icon" :> ReqBody '[JSON] FileIconVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesUpdateIcon' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "permissions" :> ReqBody '[JSON] UpdateFilePermissionsVM :> Verb 'POST 204 '[JSON] NoContent -- 'templatesUpdatePermissions' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "File" :> Capture "id" Text :> "UpdateTags" :> ReqBody '[JSON] FileTagsUpdateVM :> Verb 'PUT 200 '[JSON] TemplateVM -- 'templatesUpdateTags' route
    :<|> Protected :> "api" :> "rp" :> "v1" :> "Templates" :> "Folder" :> Capture "id" Text :> "File" :> ReqBody '[JSON] TemplateCreateVM :> Verb 'POST 200 '[JSON] TemplateVM -- 'templatesUploadFile' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "notifications" :> ReqBody '[JSON] ClearNotificationsVM :> Verb 'DELETE 204 '[JSON] NoContent -- 'userNotificationsClearNotifications' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "notifications" :> QueryParam "skip" Int :> QueryParam "take" Int :> QueryParam "subscriptionId" Text :> Verb 'GET 200 '[JSON] AuditActionsVM -- 'userNotificationsGetNotifications' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserProfile" :> Verb 'GET 200 '[JSON] UserProfileVM -- 'userProfileGetMyProfile' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserProfile" :> Capture "userId" Text :> Verb 'GET 200 '[JSON] UserProfileVM -- 'userProfileGetUserProfile' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserProfile" :> ReqBody '[JSON] UpdateUserProfileVM :> Verb 'PUT 204 '[JSON] NoContent -- 'userProfileUpdateMyProfile' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserSettings" :> "accept" :> ReqBody '[JSON] AcceptAgreementsVM :> Verb 'POST 204 '[JSON] NoContent -- 'userSettingsAcceptAgreements' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserSettings" :> Verb 'GET 200 '[JSON] UserSettingsVM -- 'userSettingsGetCurrentUserSettings' route
    :<|> Protected :> "api" :> "manage" :> "v1" :> "UserSettings" :> ReqBody '[JSON] UpdateUserSettingsVM :> Verb 'PUT 200 '[JSON] UserSettingsVM -- 'userSettingsUpdateMySettings' route
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
data FastReportCloudBackend a m = FastReportCloudBackend
  { apiKeysCreateApiKey :: a -> CreateApiKeyVM -> m ApiKeyVM{- ^  -}
  , apiKeysDeleteApiKey :: a -> DeleteApiKeyVM -> m NoContent{- ^  -}
  , apiKeysGetApiKeys :: a -> m ApiKeysVM{- ^ Always work, it should make only 200 response (except if user is not authorized). -}
  , configurationGet :: a -> m ServerConfigurationVM{- ^  -}
  , contactGroupsCreate :: a -> CreateContactGroupVM -> m ContactGroupVM{- ^  -}
  , contactGroupsDelete :: a -> Text -> m NoContent{- ^  -}
  , contactGroupsGet :: a -> Text -> m ContactGroupVM{- ^  -}
  , contactGroupsGetList :: a -> Text -> Maybe Int -> Maybe Int -> m ContactGroupsVM{- ^  -}
  , contactGroupsUpdate :: a -> Text -> UpdateContactGroupVM -> m ContactGroupVM{- ^  -}
  , contactsCreate :: a -> CreateContactVM -> m ContactVM{- ^  -}
  , contactsDelete :: a -> Text -> m NoContent{- ^  -}
  , contactsGet :: a -> Text -> m ContactVM{- ^  -}
  , contactsGetByGroup :: a -> Text -> Maybe Int -> Maybe Int -> m ContactsVM{- ^  -}
  , contactsGetList :: a -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> m ContactsVM{- ^  -}
  , contactsUpdate :: a -> Text -> UpdateContactVM -> m ContactVM{- ^  -}
  , dataSourcesCountDataSourcesAsync :: a -> Text -> m Integer{- ^  -}
  , dataSourcesCreateDataSource :: a -> CreateDataSourceVM -> m DataSourceVM{- ^  -}
  , dataSourcesDeleteDataSource :: a -> Text -> m NoContent{- ^  -}
  , dataSourcesFetchData :: a -> Text -> m NoContent{- ^  -}
  , dataSourcesGetAvailableDataSources :: a -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe DataSourceSorting -> Maybe Bool -> m DataSourcesVM{- ^  -}
  , dataSourcesGetDataSource :: a -> Text -> m DataSourceVM{- ^  -}
  , dataSourcesGetPermissions :: a -> Text -> m DataSourcePermissionsVM{- ^  -}
  , dataSourcesRenameDataSource :: a -> Text -> RenameDataSourceVM -> m DataSourceVM{- ^  -}
  , dataSourcesUpdateConnectionString :: a -> Text -> UpdateDataSourceConnectionStringVM -> m DataSourceVM{- ^  -}
  , dataSourcesUpdatePermissions :: a -> Text -> UpdateDataSourcePermissionsVM -> m NoContent{- ^  -}
  , dataSourcesUpdateSubscriptionDataSource :: a -> Text -> UpdateDataSourceSubscriptionVM -> m NoContent{- ^  -}
  , downloadGetExport :: a -> Text -> Maybe Bool -> m FilePath{- ^  -}
  , downloadGetExportThumbnail :: a -> Text -> m FilePath{- ^  -}
  , downloadGetExports :: a -> Text -> Maybe Text -> Maybe Text -> m FilePath{- ^  -}
  , downloadGetLastSVGExport :: a -> Text -> m FilePath{- ^  -}
  , downloadGetReport :: a -> Text -> m FilePath{- ^  -}
  , downloadGetReportThumbnail :: a -> Text -> m FilePath{- ^  -}
  , downloadGetReports :: a -> Text -> Maybe Text -> Maybe Text -> m FilePath{- ^  -}
  , downloadGetTemplate :: a -> Text -> m FilePath{- ^  -}
  , downloadGetTemplateThumbnail :: a -> Text -> m FilePath{- ^  -}
  , downloadGetTemplates :: a -> Text -> Maybe Text -> Maybe Text -> m FilePath{- ^  -}
  , exportFolderAndFileClearRecycleBin :: a -> Text -> m NoContent{- ^ User with a Delete RecycleBin permission can access this method. -}
  , exportFolderAndFileDeleteFiles :: a -> Text -> SelectedFilesForDeletingVM -> m NoContent{- ^ User with a Delete permission can access this method. -}
  , exportFolderAndFileGetCount :: a -> Text -> Maybe Text -> Maybe Bool -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , exportFolderAndFileGetFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFolderAndFileGetRecycleBinFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get DeletedFiles permission can access this method. -}
  , exportFolderAndFileRecoverAllFromRecycleBin :: a -> Text -> m NoContent{- ^ User with a Create RecycleBin permission can access this method. -}
  , exportFoldersCalculateFolderSize :: a -> Text -> m FolderSizeVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersCopyFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , exportFoldersDeleteFolder :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , exportFoldersGetBreadcrumbs :: a -> Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFolder :: a -> Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFolders :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetFoldersCount :: a -> Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , exportFoldersGetOrCreate :: a -> Maybe Text -> Maybe Text -> Maybe Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , exportFoldersGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , exportFoldersGetRootFolder :: a -> Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , exportFoldersMoveFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , exportFoldersMoveFolderToBin :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , exportFoldersPostFolder :: a -> Text -> ExportFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , exportFoldersRecoverFolder :: a -> Text -> Maybe Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , exportFoldersRenameFolder :: a -> Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , exportFoldersUpdateIcon :: a -> Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , exportFoldersUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , exportFoldersUpdateTags :: a -> Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , exportsCopyFile :: a -> Text -> Text -> m ExportVM{- ^  -}
  , exportsDeleteFile :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , exportsGetFile :: a -> Text -> m ExportVM{- ^ User with Get Entity permission can access this method. -}
  , exportsGetFileHistory :: a -> Text -> Maybe Int -> Maybe Int -> m AuditActionsVM{- ^  -}
  , exportsGetFilesCount :: a -> Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , exportsGetFilesList :: a -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe FileSorting -> Maybe Bool -> Maybe Bool -> m ExportsVM{- ^  -}
  , exportsGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , exportsMoveFile :: a -> Text -> Text -> m ExportVM{- ^ User with Update Place permission can access this method. -}
  , exportsMoveFileToBin :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , exportsRecoverFile :: a -> Text -> Maybe Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , exportsRenameFile :: a -> Text -> FileRenameVM -> m ExportVM{- ^ User with Update Name permission can access this method. -}
  , exportsUpdateIcon :: a -> Text -> FileIconVM -> m ExportVM{- ^ User with Update Icon permission can access this method. -}
  , exportsUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , exportsUpdateTags :: a -> Text -> FileTagsUpdateVM -> m ExportVM{- ^ User with Update Tags permission can access this method. -}
  , groupUsersAddUserToGroup :: a -> Text -> Text -> m NoContent{- ^  -}
  , groupUsersGetUsersInGroup :: a -> Text -> Maybe Int -> Maybe Int -> m GroupUsersVM{- ^  -}
  , groupUsersLeaveFromGroup :: a -> Text -> m NoContent{- ^  -}
  , groupUsersRemoveFromGroup :: a -> Text -> Text -> m NoContent{- ^  -}
  , groupsCreateGroup :: a -> CreateGroupVM -> m GroupVM{- ^  -}
  , groupsDeleteGroup :: a -> Text -> m NoContent{- ^  -}
  , groupsGetGroup :: a -> Text -> m GroupVM{- ^  -}
  , groupsGetGroupList :: a -> Maybe Int -> Maybe Int -> m GroupsVM{- ^  -}
  , groupsGetPermissions :: a -> Text -> m GroupPermissionsVM{- ^  -}
  , groupsRenameGroup :: a -> Text -> RenameGroupVM -> m GroupVM{- ^  -}
  , groupsUpdatePermissions :: a -> Text -> UpdateGroupPermissionsVM -> m NoContent{- ^  -}
  , healthCheckDataGet :: a -> m NoContent{- ^  -}
  , reportFolderAndFileClearRecycleBin :: a -> Text -> m NoContent{- ^ User with a Delete RecycleBin permission can access this method. -}
  , reportFolderAndFileDeleteFiles :: a -> Text -> SelectedFilesForDeletingVM -> m NoContent{- ^ User with a Delete permission can access this method. -}
  , reportFolderAndFileGetCount :: a -> Text -> Maybe Text -> Maybe Bool -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , reportFolderAndFileGetFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFolderAndFileGetRecycleBinFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get DeletedFiles permission can access this method. -}
  , reportFolderAndFileRecoverAllFromRecycleBin :: a -> Text -> m NoContent{- ^ User with a Create RecycleBin permission can access this method. -}
  , reportFoldersCalculateFolderSize :: a -> Text -> m FolderSizeVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersCopyFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , reportFoldersDeleteFolder :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , reportFoldersExport :: a -> Text -> ExportReportVM -> m FileVM{- ^ User with Execute Export permission on report folder and  Create Entity on an export folder can access this method. -}
  , reportFoldersGetBreadcrumbs :: a -> Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFolder :: a -> Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFolders :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetFoldersCount :: a -> Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , reportFoldersGetOrCreate :: a -> Maybe Text -> Maybe Text -> Maybe Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , reportFoldersGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , reportFoldersGetRootFolder :: a -> Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , reportFoldersMoveFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , reportFoldersMoveFolderToBin :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , reportFoldersPostFolder :: a -> Text -> ReportFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , reportFoldersRecoverFolder :: a -> Text -> Maybe Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , reportFoldersRenameFolder :: a -> Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , reportFoldersUpdateIcon :: a -> Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , reportFoldersUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , reportFoldersUpdateTags :: a -> Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , reportsCopyFile :: a -> Text -> Text -> m ReportVM{- ^  -}
  , reportsDeleteFile :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , reportsExport :: a -> Text -> ExportReportVM -> m ExportVM{- ^ User with Execute Export permission on prepared report and  Create Entity on an export folder can access this method. -}
  , reportsGetFile :: a -> Text -> m ReportVM{- ^ User with Get Entity permission can access this method. -}
  , reportsGetFileHistory :: a -> Text -> Maybe Int -> Maybe Int -> m AuditActionsVM{- ^  -}
  , reportsGetFilesCount :: a -> Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , reportsGetFilesList :: a -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe FileSorting -> Maybe Bool -> Maybe Bool -> m ReportsVM{- ^  -}
  , reportsGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , reportsMoveFile :: a -> Text -> Text -> m ReportVM{- ^ User with Update Place permission can access this method. -}
  , reportsMoveFileToBin :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , reportsRecoverFile :: a -> Text -> Maybe Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , reportsRenameFile :: a -> Text -> FileRenameVM -> m ReportVM{- ^ User with Update Name permission can access this method. -}
  , reportsStaticPreview :: a -> Text -> PreviewReportVM -> m ExportVM{- ^  -}
  , reportsUpdateIcon :: a -> Text -> FileIconVM -> m ReportVM{- ^ User with Update Icon permission can access this method. -}
  , reportsUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , reportsUpdateTags :: a -> Text -> FileTagsUpdateVM -> m ReportVM{- ^ User with Update Tags permission can access this method. -}
  , reportsUploadFile :: a -> Text -> ReportCreateVM -> m ReportVM{- ^ User with Create Entity permission can access this method. -}
  , subscriptionGroupsCountGroupsAsync :: a -> Text -> m Integer{- ^  -}
  , subscriptionGroupsGetGroupsList :: a -> Text -> Maybe Text -> m GroupsVM{- ^  -}
  , subscriptionInvitesAcceptInvite :: a -> Text -> Text -> m NoContent{- ^  -}
  , subscriptionInvitesCreateInvite :: a -> Text -> CreateSubscriptionInviteVM -> m SubscriptionInviteVM{- ^  -}
  , subscriptionInvitesDeleteInvite :: a -> Text -> Text -> m NoContent{- ^  -}
  , subscriptionInvitesGetInvites :: a -> Text -> m SubscriptionInvitesVM{- ^  -}
  , subscriptionPlansGetSubscriptionPlan :: a -> Text -> m SubscriptionPlanVM{- ^  -}
  , subscriptionPlansGetSubscriptionPlans :: a -> Maybe Int -> Maybe Int -> m SubscriptionPlansVM{- ^ If no active subscription plans, then the endpoint will return empty list -}
  , subscriptionUsersAddUser :: a -> Text -> Text -> m NoContent{- ^  -}
  , subscriptionUsersCountUsersAsync :: a -> Text -> m Integer{- ^  -}
  , subscriptionUsersGetUsers :: a -> Text -> Maybe Int -> Maybe Int -> m SubscriptionUsersVM{- ^  -}
  , subscriptionUsersLeaveSubscripiton :: a -> Text -> m NoContent{- ^  -}
  , subscriptionUsersRemoveUser :: a -> Text -> Text -> m NoContent{- ^  -}
  , subscriptionsGetDefaultPermissions :: a -> Text -> m DefaultPermissionsVM{- ^  -}
  , subscriptionsGetMyPermissions :: a -> Text -> m MyPermissionsVM{- ^  -}
  , subscriptionsGetPermissions :: a -> Text -> m SubscriptionPermissionsVM{- ^  -}
  , subscriptionsGetSubscription :: a -> Text -> m SubscriptionVM{- ^  -}
  , subscriptionsGetSubscriptions :: a -> Maybe Int -> Maybe Int -> m SubscriptionsVM{- ^  -}
  , subscriptionsRenameSubscription :: a -> Text -> RenameSubscriptionVM -> m SubscriptionVM{- ^  -}
  , subscriptionsUpdateDefaultPermissions :: a -> Text -> UpdateDefaultPermissionsVM -> m DefaultPermissionsVM{- ^  -}
  , subscriptionsUpdateLocale :: a -> Text -> UpdateSubscriptionLocaleVM -> m SubscriptionVM{- ^  -}
  , subscriptionsUpdatePermissions :: a -> Text -> UpdateSubscriptionPermissionsVM -> m NoContent{- ^  -}
  , tasksCreateTask :: a -> CreateTaskBaseVM -> m TaskBaseVM{- ^  -}
  , tasksDeleteTask :: a -> Text -> m NoContent{- ^  -}
  , tasksGet :: a -> Text -> m TaskBaseVM{- ^  -}
  , tasksGetList :: a -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> m TasksVM{- ^  -}
  , tasksGetPermissions :: a -> Text -> m TaskPermissionsVM{- ^  -}
  , tasksRenameTask :: a -> Text -> Maybe Text -> m TaskBaseVM{- ^  -}
  , tasksRunTask :: a -> RunTaskBaseVM -> m NoContent{- ^  -}
  , tasksRunTaskById :: a -> Text -> m NoContent{- ^  -}
  , tasksUpdatePermissions :: a -> Text -> UpdateTaskPermissionsVM -> m NoContent{- ^  -}
  , tasksUpdateTask :: a -> Text -> UpdateTaskBaseVM -> m TaskBaseVM{- ^  -}
  , templateFolderAndFileClearRecycleBin :: a -> Text -> m NoContent{- ^ User with a Delete RecycleBin permission can access this method. -}
  , templateFolderAndFileDeleteFiles :: a -> Text -> SelectedFilesForDeletingVM -> m NoContent{- ^ User with a Delete permission can access this method. -}
  , templateFolderAndFileGetCount :: a -> Text -> Maybe Text -> Maybe Bool -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , templateFolderAndFileGetFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFolderAndFileGetRecycleBinFoldersAndFiles :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get DeletedFiles permission can access this method. -}
  , templateFolderAndFileRecoverAllFromRecycleBin :: a -> Text -> m NoContent{- ^ User with a Create RecycleBin permission can access this method. -}
  , templateFoldersCalculateFolderSize :: a -> Text -> m FolderSizeVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersCopyFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , templateFoldersDeleteFolder :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , templateFoldersExport :: a -> Text -> ExportTemplateVM -> m FileVM{- ^ User with Execute Export permission on template folder and  Create Entity on an export folder can access this method. -}
  , templateFoldersGetBreadcrumbs :: a -> Text -> m BreadcrumbsVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFolder :: a -> Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFolders :: a -> Text -> Maybe Int -> Maybe Int -> Maybe FileSorting -> Maybe Bool -> Maybe Text -> Maybe Bool -> m FilesVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetFoldersCount :: a -> Text -> m CountVM{- ^ User with a Get Count permission can access this method. -}
  , templateFoldersGetOrCreate :: a -> Maybe Text -> Maybe Text -> Maybe Text -> m FileVM{- ^ User with a Get Entity permission can access this method. -}
  , templateFoldersGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , templateFoldersGetRootFolder :: a -> Maybe Text -> m FileVM{- ^ > Breakchange. Now user model doesn't contain a root folders.  This method can return error 400 and 404 when subscription is not found. -}
  , templateFoldersMoveFolder :: a -> Text -> Text -> m FileVM{- ^ User with a Update Place permission for a folder and Create Entity  for a Parent Folder can access this method. -}
  , templateFoldersMoveFolderToBin :: a -> Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , templateFoldersPostFolder :: a -> Text -> TemplateFolderCreateVM -> m FileVM{- ^ User with a Create Entity permisison can access this method. -}
  , templateFoldersPrepare :: a -> Text -> PrepareTemplateVM -> m FileVM{- ^ User with Execute Prepare permission on report and  Create Entity on a prepared report folder can access this method. -}
  , templateFoldersRecoverFolder :: a -> Text -> Maybe Text -> m NoContent{- ^ User with a Delete Entity permission can access this method. -}
  , templateFoldersRenameFolder :: a -> Text -> FolderRenameVM -> m FileVM{- ^ User with a Update Name permision can access this method. -}
  , templateFoldersUpdateIcon :: a -> Text -> FolderIconVM -> m FileVM{- ^ User with a Update Icon permission can access this method. -}
  , templateFoldersUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , templateFoldersUpdateTags :: a -> Text -> FolderTagsUpdateVM -> m FileVM{- ^ User with a Update Tags permission can access this method. -}
  , templatesCopyFile :: a -> Text -> Text -> m TemplateVM{- ^  -}
  , templatesDeleteFile :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , templatesExport :: a -> Text -> ExportTemplateVM -> m ExportVM{- ^ User with Execute Export permission on prepared report and  Create Entity on an export folder can access this method. -}
  , templatesGetFile :: a -> Text -> m TemplateVM{- ^ User with Get Entity permission can access this method. -}
  , templatesGetFileHistory :: a -> Text -> Maybe Int -> Maybe Int -> m AuditActionsVM{- ^  -}
  , templatesGetFilesCount :: a -> Text -> m CountVM{- ^ User with Get Count permission can access this method. -}
  , templatesGetFilesList :: a -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe FileSorting -> Maybe Bool -> Maybe Bool -> m TemplatesVM{- ^  -}
  , templatesGetPermissions :: a -> Text -> m FilePermissionsVM{- ^  -}
  , templatesMoveFile :: a -> Text -> Text -> m TemplateVM{- ^ User with Update Place permission can access this method. -}
  , templatesMoveFileToBin :: a -> Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , templatesPrepare :: a -> Text -> PrepareTemplateVM -> m ReportVM{- ^ User with Execute Prepare permission on report and  Create Entity on a prepared report folder can access this method. -}
  , templatesRecoverFile :: a -> Text -> Maybe Text -> m NoContent{- ^ User with Delete permission can access the method. -}
  , templatesRenameFile :: a -> Text -> FileRenameVM -> m TemplateVM{- ^ User with Update Name permission can access this method. -}
  , templatesStaticPreview :: a -> Text -> PreviewTemplateVM -> m ExportVM{- ^  -}
  , templatesUpdateContent :: a -> Text -> UpdateFileContentVM -> m NoContent{- ^  -}
  , templatesUpdateIcon :: a -> Text -> FileIconVM -> m TemplateVM{- ^ User with Update Icon permission can access this method. -}
  , templatesUpdatePermissions :: a -> Text -> UpdateFilePermissionsVM -> m NoContent{- ^  -}
  , templatesUpdateTags :: a -> Text -> FileTagsUpdateVM -> m TemplateVM{- ^ User with Update Tags permission can access this method. -}
  , templatesUploadFile :: a -> Text -> TemplateCreateVM -> m TemplateVM{- ^ User with Create Entity permission can access this method. -}
  , userNotificationsClearNotifications :: a -> ClearNotificationsVM -> m NoContent{- ^  -}
  , userNotificationsGetNotifications :: a -> Maybe Int -> Maybe Int -> Maybe Text -> m AuditActionsVM{- ^  -}
  , userProfileGetMyProfile :: a -> m UserProfileVM{- ^  -}
  , userProfileGetUserProfile :: a -> Text -> m UserProfileVM{- ^  -}
  , userProfileUpdateMyProfile :: a -> UpdateUserProfileVM -> m NoContent{- ^ This method is only allowed for local sign in via intranet -}
  , userSettingsAcceptAgreements :: a -> AcceptAgreementsVM -> m NoContent{- ^  -}
  , userSettingsGetCurrentUserSettings :: a -> m UserSettingsVM{- ^  -}
  , userSettingsUpdateMySettings :: a -> UpdateUserSettingsVM -> m UserSettingsVM{- ^  -}
  }

-- | Authentication settings for FastReportCloud.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FastReportCloudAuth = FastReportCloudAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }
-- | Authentication settings for FastReportCloud.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data FastReportCloudAuth = FastReportCloudAuth
  { lookupUser :: BasicAuthData -> Handler AuthServer
  , authError :: Request -> ServerError
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

createFastReportCloudClient :: FastReportCloudBackend AuthClient FastReportCloudClient
createFastReportCloudClient = FastReportCloudBackend{..}
  where
    ((coerce -> apiKeysCreateApiKey) :<|>
     (coerce -> apiKeysDeleteApiKey) :<|>
     (coerce -> apiKeysGetApiKeys) :<|>
     (coerce -> configurationGet) :<|>
     (coerce -> contactGroupsCreate) :<|>
     (coerce -> contactGroupsDelete) :<|>
     (coerce -> contactGroupsGet) :<|>
     (coerce -> contactGroupsGetList) :<|>
     (coerce -> contactGroupsUpdate) :<|>
     (coerce -> contactsCreate) :<|>
     (coerce -> contactsDelete) :<|>
     (coerce -> contactsGet) :<|>
     (coerce -> contactsGetByGroup) :<|>
     (coerce -> contactsGetList) :<|>
     (coerce -> contactsUpdate) :<|>
     (coerce -> dataSourcesCountDataSourcesAsync) :<|>
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
     (coerce -> downloadGetLastSVGExport) :<|>
     (coerce -> downloadGetReport) :<|>
     (coerce -> downloadGetReportThumbnail) :<|>
     (coerce -> downloadGetReports) :<|>
     (coerce -> downloadGetTemplate) :<|>
     (coerce -> downloadGetTemplateThumbnail) :<|>
     (coerce -> downloadGetTemplates) :<|>
     (coerce -> exportFolderAndFileClearRecycleBin) :<|>
     (coerce -> exportFolderAndFileDeleteFiles) :<|>
     (coerce -> exportFolderAndFileGetCount) :<|>
     (coerce -> exportFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> exportFolderAndFileGetRecycleBinFoldersAndFiles) :<|>
     (coerce -> exportFolderAndFileRecoverAllFromRecycleBin) :<|>
     (coerce -> exportFoldersCalculateFolderSize) :<|>
     (coerce -> exportFoldersCopyFolder) :<|>
     (coerce -> exportFoldersDeleteFolder) :<|>
     (coerce -> exportFoldersGetBreadcrumbs) :<|>
     (coerce -> exportFoldersGetFolder) :<|>
     (coerce -> exportFoldersGetFolders) :<|>
     (coerce -> exportFoldersGetFoldersCount) :<|>
     (coerce -> exportFoldersGetOrCreate) :<|>
     (coerce -> exportFoldersGetPermissions) :<|>
     (coerce -> exportFoldersGetRootFolder) :<|>
     (coerce -> exportFoldersMoveFolder) :<|>
     (coerce -> exportFoldersMoveFolderToBin) :<|>
     (coerce -> exportFoldersPostFolder) :<|>
     (coerce -> exportFoldersRecoverFolder) :<|>
     (coerce -> exportFoldersRenameFolder) :<|>
     (coerce -> exportFoldersUpdateIcon) :<|>
     (coerce -> exportFoldersUpdatePermissions) :<|>
     (coerce -> exportFoldersUpdateTags) :<|>
     (coerce -> exportsCopyFile) :<|>
     (coerce -> exportsDeleteFile) :<|>
     (coerce -> exportsGetFile) :<|>
     (coerce -> exportsGetFileHistory) :<|>
     (coerce -> exportsGetFilesCount) :<|>
     (coerce -> exportsGetFilesList) :<|>
     (coerce -> exportsGetPermissions) :<|>
     (coerce -> exportsMoveFile) :<|>
     (coerce -> exportsMoveFileToBin) :<|>
     (coerce -> exportsRecoverFile) :<|>
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
     (coerce -> reportFolderAndFileClearRecycleBin) :<|>
     (coerce -> reportFolderAndFileDeleteFiles) :<|>
     (coerce -> reportFolderAndFileGetCount) :<|>
     (coerce -> reportFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> reportFolderAndFileGetRecycleBinFoldersAndFiles) :<|>
     (coerce -> reportFolderAndFileRecoverAllFromRecycleBin) :<|>
     (coerce -> reportFoldersCalculateFolderSize) :<|>
     (coerce -> reportFoldersCopyFolder) :<|>
     (coerce -> reportFoldersDeleteFolder) :<|>
     (coerce -> reportFoldersExport) :<|>
     (coerce -> reportFoldersGetBreadcrumbs) :<|>
     (coerce -> reportFoldersGetFolder) :<|>
     (coerce -> reportFoldersGetFolders) :<|>
     (coerce -> reportFoldersGetFoldersCount) :<|>
     (coerce -> reportFoldersGetOrCreate) :<|>
     (coerce -> reportFoldersGetPermissions) :<|>
     (coerce -> reportFoldersGetRootFolder) :<|>
     (coerce -> reportFoldersMoveFolder) :<|>
     (coerce -> reportFoldersMoveFolderToBin) :<|>
     (coerce -> reportFoldersPostFolder) :<|>
     (coerce -> reportFoldersRecoverFolder) :<|>
     (coerce -> reportFoldersRenameFolder) :<|>
     (coerce -> reportFoldersUpdateIcon) :<|>
     (coerce -> reportFoldersUpdatePermissions) :<|>
     (coerce -> reportFoldersUpdateTags) :<|>
     (coerce -> reportsCopyFile) :<|>
     (coerce -> reportsDeleteFile) :<|>
     (coerce -> reportsExport) :<|>
     (coerce -> reportsGetFile) :<|>
     (coerce -> reportsGetFileHistory) :<|>
     (coerce -> reportsGetFilesCount) :<|>
     (coerce -> reportsGetFilesList) :<|>
     (coerce -> reportsGetPermissions) :<|>
     (coerce -> reportsMoveFile) :<|>
     (coerce -> reportsMoveFileToBin) :<|>
     (coerce -> reportsRecoverFile) :<|>
     (coerce -> reportsRenameFile) :<|>
     (coerce -> reportsStaticPreview) :<|>
     (coerce -> reportsUpdateIcon) :<|>
     (coerce -> reportsUpdatePermissions) :<|>
     (coerce -> reportsUpdateTags) :<|>
     (coerce -> reportsUploadFile) :<|>
     (coerce -> subscriptionGroupsCountGroupsAsync) :<|>
     (coerce -> subscriptionGroupsGetGroupsList) :<|>
     (coerce -> subscriptionInvitesAcceptInvite) :<|>
     (coerce -> subscriptionInvitesCreateInvite) :<|>
     (coerce -> subscriptionInvitesDeleteInvite) :<|>
     (coerce -> subscriptionInvitesGetInvites) :<|>
     (coerce -> subscriptionPlansGetSubscriptionPlan) :<|>
     (coerce -> subscriptionPlansGetSubscriptionPlans) :<|>
     (coerce -> subscriptionUsersAddUser) :<|>
     (coerce -> subscriptionUsersCountUsersAsync) :<|>
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
     (coerce -> tasksGetPermissions) :<|>
     (coerce -> tasksRenameTask) :<|>
     (coerce -> tasksRunTask) :<|>
     (coerce -> tasksRunTaskById) :<|>
     (coerce -> tasksUpdatePermissions) :<|>
     (coerce -> tasksUpdateTask) :<|>
     (coerce -> templateFolderAndFileClearRecycleBin) :<|>
     (coerce -> templateFolderAndFileDeleteFiles) :<|>
     (coerce -> templateFolderAndFileGetCount) :<|>
     (coerce -> templateFolderAndFileGetFoldersAndFiles) :<|>
     (coerce -> templateFolderAndFileGetRecycleBinFoldersAndFiles) :<|>
     (coerce -> templateFolderAndFileRecoverAllFromRecycleBin) :<|>
     (coerce -> templateFoldersCalculateFolderSize) :<|>
     (coerce -> templateFoldersCopyFolder) :<|>
     (coerce -> templateFoldersDeleteFolder) :<|>
     (coerce -> templateFoldersExport) :<|>
     (coerce -> templateFoldersGetBreadcrumbs) :<|>
     (coerce -> templateFoldersGetFolder) :<|>
     (coerce -> templateFoldersGetFolders) :<|>
     (coerce -> templateFoldersGetFoldersCount) :<|>
     (coerce -> templateFoldersGetOrCreate) :<|>
     (coerce -> templateFoldersGetPermissions) :<|>
     (coerce -> templateFoldersGetRootFolder) :<|>
     (coerce -> templateFoldersMoveFolder) :<|>
     (coerce -> templateFoldersMoveFolderToBin) :<|>
     (coerce -> templateFoldersPostFolder) :<|>
     (coerce -> templateFoldersPrepare) :<|>
     (coerce -> templateFoldersRecoverFolder) :<|>
     (coerce -> templateFoldersRenameFolder) :<|>
     (coerce -> templateFoldersUpdateIcon) :<|>
     (coerce -> templateFoldersUpdatePermissions) :<|>
     (coerce -> templateFoldersUpdateTags) :<|>
     (coerce -> templatesCopyFile) :<|>
     (coerce -> templatesDeleteFile) :<|>
     (coerce -> templatesExport) :<|>
     (coerce -> templatesGetFile) :<|>
     (coerce -> templatesGetFileHistory) :<|>
     (coerce -> templatesGetFilesCount) :<|>
     (coerce -> templatesGetFilesList) :<|>
     (coerce -> templatesGetPermissions) :<|>
     (coerce -> templatesMoveFile) :<|>
     (coerce -> templatesMoveFileToBin) :<|>
     (coerce -> templatesPrepare) :<|>
     (coerce -> templatesRecoverFile) :<|>
     (coerce -> templatesRenameFile) :<|>
     (coerce -> templatesStaticPreview) :<|>
     (coerce -> templatesUpdateContent) :<|>
     (coerce -> templatesUpdateIcon) :<|>
     (coerce -> templatesUpdatePermissions) :<|>
     (coerce -> templatesUpdateTags) :<|>
     (coerce -> templatesUploadFile) :<|>
     (coerce -> userNotificationsClearNotifications) :<|>
     (coerce -> userNotificationsGetNotifications) :<|>
     (coerce -> userProfileGetMyProfile) :<|>
     (coerce -> userProfileGetUserProfile) :<|>
     (coerce -> userProfileUpdateMyProfile) :<|>
     (coerce -> userSettingsAcceptAgreements) :<|>
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
  => Config -> FastReportCloudAuth -> FastReportCloudBackend AuthServer (ExceptT ServerError IO) -> m ()
runFastReportCloudServer config auth backend = runFastReportCloudMiddlewareServer config requestMiddlewareId auth backend

-- | Run the FastReportCloud server at the provided host and port.
runFastReportCloudMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> FastReportCloudAuth -> FastReportCloudBackend AuthServer (ExceptT ServerError IO) -> m ()
runFastReportCloudMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationFastReportCloud auth backend

-- | Plain "Network.Wai" Application for the FastReportCloud server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationFastReportCloud :: FastReportCloudAuth -> FastReportCloudBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationFastReportCloud auth backend = serveWithContextT (Proxy :: Proxy FastReportCloudAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend FastReportCloudBackend{..} =
      (coerce apiKeysCreateApiKey :<|>
       coerce apiKeysDeleteApiKey :<|>
       coerce apiKeysGetApiKeys :<|>
       coerce configurationGet :<|>
       coerce contactGroupsCreate :<|>
       coerce contactGroupsDelete :<|>
       coerce contactGroupsGet :<|>
       coerce contactGroupsGetList :<|>
       coerce contactGroupsUpdate :<|>
       coerce contactsCreate :<|>
       coerce contactsDelete :<|>
       coerce contactsGet :<|>
       coerce contactsGetByGroup :<|>
       coerce contactsGetList :<|>
       coerce contactsUpdate :<|>
       coerce dataSourcesCountDataSourcesAsync :<|>
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
       coerce downloadGetLastSVGExport :<|>
       coerce downloadGetReport :<|>
       coerce downloadGetReportThumbnail :<|>
       coerce downloadGetReports :<|>
       coerce downloadGetTemplate :<|>
       coerce downloadGetTemplateThumbnail :<|>
       coerce downloadGetTemplates :<|>
       coerce exportFolderAndFileClearRecycleBin :<|>
       coerce exportFolderAndFileDeleteFiles :<|>
       coerce exportFolderAndFileGetCount :<|>
       coerce exportFolderAndFileGetFoldersAndFiles :<|>
       coerce exportFolderAndFileGetRecycleBinFoldersAndFiles :<|>
       coerce exportFolderAndFileRecoverAllFromRecycleBin :<|>
       coerce exportFoldersCalculateFolderSize :<|>
       coerce exportFoldersCopyFolder :<|>
       coerce exportFoldersDeleteFolder :<|>
       coerce exportFoldersGetBreadcrumbs :<|>
       coerce exportFoldersGetFolder :<|>
       coerce exportFoldersGetFolders :<|>
       coerce exportFoldersGetFoldersCount :<|>
       coerce exportFoldersGetOrCreate :<|>
       coerce exportFoldersGetPermissions :<|>
       coerce exportFoldersGetRootFolder :<|>
       coerce exportFoldersMoveFolder :<|>
       coerce exportFoldersMoveFolderToBin :<|>
       coerce exportFoldersPostFolder :<|>
       coerce exportFoldersRecoverFolder :<|>
       coerce exportFoldersRenameFolder :<|>
       coerce exportFoldersUpdateIcon :<|>
       coerce exportFoldersUpdatePermissions :<|>
       coerce exportFoldersUpdateTags :<|>
       coerce exportsCopyFile :<|>
       coerce exportsDeleteFile :<|>
       coerce exportsGetFile :<|>
       coerce exportsGetFileHistory :<|>
       coerce exportsGetFilesCount :<|>
       coerce exportsGetFilesList :<|>
       coerce exportsGetPermissions :<|>
       coerce exportsMoveFile :<|>
       coerce exportsMoveFileToBin :<|>
       coerce exportsRecoverFile :<|>
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
       coerce reportFolderAndFileClearRecycleBin :<|>
       coerce reportFolderAndFileDeleteFiles :<|>
       coerce reportFolderAndFileGetCount :<|>
       coerce reportFolderAndFileGetFoldersAndFiles :<|>
       coerce reportFolderAndFileGetRecycleBinFoldersAndFiles :<|>
       coerce reportFolderAndFileRecoverAllFromRecycleBin :<|>
       coerce reportFoldersCalculateFolderSize :<|>
       coerce reportFoldersCopyFolder :<|>
       coerce reportFoldersDeleteFolder :<|>
       coerce reportFoldersExport :<|>
       coerce reportFoldersGetBreadcrumbs :<|>
       coerce reportFoldersGetFolder :<|>
       coerce reportFoldersGetFolders :<|>
       coerce reportFoldersGetFoldersCount :<|>
       coerce reportFoldersGetOrCreate :<|>
       coerce reportFoldersGetPermissions :<|>
       coerce reportFoldersGetRootFolder :<|>
       coerce reportFoldersMoveFolder :<|>
       coerce reportFoldersMoveFolderToBin :<|>
       coerce reportFoldersPostFolder :<|>
       coerce reportFoldersRecoverFolder :<|>
       coerce reportFoldersRenameFolder :<|>
       coerce reportFoldersUpdateIcon :<|>
       coerce reportFoldersUpdatePermissions :<|>
       coerce reportFoldersUpdateTags :<|>
       coerce reportsCopyFile :<|>
       coerce reportsDeleteFile :<|>
       coerce reportsExport :<|>
       coerce reportsGetFile :<|>
       coerce reportsGetFileHistory :<|>
       coerce reportsGetFilesCount :<|>
       coerce reportsGetFilesList :<|>
       coerce reportsGetPermissions :<|>
       coerce reportsMoveFile :<|>
       coerce reportsMoveFileToBin :<|>
       coerce reportsRecoverFile :<|>
       coerce reportsRenameFile :<|>
       coerce reportsStaticPreview :<|>
       coerce reportsUpdateIcon :<|>
       coerce reportsUpdatePermissions :<|>
       coerce reportsUpdateTags :<|>
       coerce reportsUploadFile :<|>
       coerce subscriptionGroupsCountGroupsAsync :<|>
       coerce subscriptionGroupsGetGroupsList :<|>
       coerce subscriptionInvitesAcceptInvite :<|>
       coerce subscriptionInvitesCreateInvite :<|>
       coerce subscriptionInvitesDeleteInvite :<|>
       coerce subscriptionInvitesGetInvites :<|>
       coerce subscriptionPlansGetSubscriptionPlan :<|>
       coerce subscriptionPlansGetSubscriptionPlans :<|>
       coerce subscriptionUsersAddUser :<|>
       coerce subscriptionUsersCountUsersAsync :<|>
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
       coerce tasksGetPermissions :<|>
       coerce tasksRenameTask :<|>
       coerce tasksRunTask :<|>
       coerce tasksRunTaskById :<|>
       coerce tasksUpdatePermissions :<|>
       coerce tasksUpdateTask :<|>
       coerce templateFolderAndFileClearRecycleBin :<|>
       coerce templateFolderAndFileDeleteFiles :<|>
       coerce templateFolderAndFileGetCount :<|>
       coerce templateFolderAndFileGetFoldersAndFiles :<|>
       coerce templateFolderAndFileGetRecycleBinFoldersAndFiles :<|>
       coerce templateFolderAndFileRecoverAllFromRecycleBin :<|>
       coerce templateFoldersCalculateFolderSize :<|>
       coerce templateFoldersCopyFolder :<|>
       coerce templateFoldersDeleteFolder :<|>
       coerce templateFoldersExport :<|>
       coerce templateFoldersGetBreadcrumbs :<|>
       coerce templateFoldersGetFolder :<|>
       coerce templateFoldersGetFolders :<|>
       coerce templateFoldersGetFoldersCount :<|>
       coerce templateFoldersGetOrCreate :<|>
       coerce templateFoldersGetPermissions :<|>
       coerce templateFoldersGetRootFolder :<|>
       coerce templateFoldersMoveFolder :<|>
       coerce templateFoldersMoveFolderToBin :<|>
       coerce templateFoldersPostFolder :<|>
       coerce templateFoldersPrepare :<|>
       coerce templateFoldersRecoverFolder :<|>
       coerce templateFoldersRenameFolder :<|>
       coerce templateFoldersUpdateIcon :<|>
       coerce templateFoldersUpdatePermissions :<|>
       coerce templateFoldersUpdateTags :<|>
       coerce templatesCopyFile :<|>
       coerce templatesDeleteFile :<|>
       coerce templatesExport :<|>
       coerce templatesGetFile :<|>
       coerce templatesGetFileHistory :<|>
       coerce templatesGetFilesCount :<|>
       coerce templatesGetFilesList :<|>
       coerce templatesGetPermissions :<|>
       coerce templatesMoveFile :<|>
       coerce templatesMoveFileToBin :<|>
       coerce templatesPrepare :<|>
       coerce templatesRecoverFile :<|>
       coerce templatesRenameFile :<|>
       coerce templatesStaticPreview :<|>
       coerce templatesUpdateContent :<|>
       coerce templatesUpdateIcon :<|>
       coerce templatesUpdatePermissions :<|>
       coerce templatesUpdateTags :<|>
       coerce templatesUploadFile :<|>
       coerce userNotificationsClearNotifications :<|>
       coerce userNotificationsGetNotifications :<|>
       coerce userProfileGetMyProfile :<|>
       coerce userProfileGetUserProfile :<|>
       coerce userProfileUpdateMyProfile :<|>
       coerce userSettingsAcceptAgreements :<|>
       coerce userSettingsGetCurrentUserSettings :<|>
       coerce userSettingsUpdateMySettings :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FastReportCloudAuth -> AuthHandler Request AuthServer
authHandler FastReportCloudAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBearerAuth header of
        Just key -> lookupUser key
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "bearer"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest ("Bearer " <> key) (addHeader "Authorization")
-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: FastReportCloudAuth -> AuthHandler Request AuthServer
authHandler FastReportCloudAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBasicAuth header of
        Just (user, password) -> lookupUser (BasicAuthData user password)
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "basic"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = BasicAuthData

clientAuth :: BasicAuthData -> AuthClient
clientAuth key = mkAuthenticatedRequest key basicAuthReq

serverContext :: FastReportCloudAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
