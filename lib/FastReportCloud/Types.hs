{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module FastReportCloud.Types (
  ApiKeyVM (..),
  ApiKeysVM (..),
  BreadcrumbsModel (..),
  BreadcrumbsVM (..),
  CountVM (..),
  CreateApiKeyVM (..),
  CreateDataSourceVM (..),
  CreateGroupVM (..),
  CreateSubscriptionInviteVM (..),
  DataSourcePermission (..),
  DataSourcePermissions (..),
  DataSourcePermissionsVM (..),
  DataSourceVM (..),
  DataSourcesVM (..),
  DefaultPermissions (..),
  DefaultPermissionsVM (..),
  DeleteApiKeyVM (..),
  ExportFolderCreateVM (..),
  ExportReportTaskVM (..),
  ExportTemplateTaskVM (..),
  ExportVM (..),
  ExportsVM (..),
  FileIconVM (..),
  FilePermission (..),
  FilePermissions (..),
  FilePermissionsVM (..),
  FileRenameVM (..),
  FileTagsUpdateVM (..),
  FileVM (..),
  FilesVM (..),
  FolderIconVM (..),
  FolderRenameVM (..),
  FolderTagsUpdateVM (..),
  GroupPermission (..),
  GroupPermissions (..),
  GroupPermissionsVM (..),
  GroupUserVM (..),
  GroupUsersVM (..),
  GroupVM (..),
  GroupsVM (..),
  InvitedUser (..),
  PrepareTemplateTaskVM (..),
  ProblemDetails (..),
  RenameDataSourceVM (..),
  RenameGroupVM (..),
  RenameSubscriptionVM (..),
  ReportCreateVM (..),
  ReportFolderCreateVM (..),
  ReportInfo (..),
  ReportVM (..),
  ReportsVM (..),
  SubscriptionFolder (..),
  SubscriptionInviteVM (..),
  SubscriptionInvitesVM (..),
  SubscriptionPeriodVM (..),
  SubscriptionPermission (..),
  SubscriptionPermissions (..),
  SubscriptionPermissionsVM (..),
  SubscriptionPlanVM (..),
  SubscriptionPlansVM (..),
  SubscriptionUserVM (..),
  SubscriptionUsersVM (..),
  SubscriptionVM (..),
  SubscriptionsVM (..),
  TemplateCreateVM (..),
  TemplateFolderCreateVM (..),
  TemplateVM (..),
  TemplatesVM (..),
  UpdateDataSourceConnectionStringVM (..),
  UpdateDataSourcePermissionsVM (..),
  UpdateDataSourceSubscriptionVM (..),
  UpdateDefaultPermissionsVM (..),
  UpdateFilePermissionsVM (..),
  UpdateGroupPermissionsVM (..),
  UpdateSubscriptionLocaleVM (..),
  UpdateSubscriptionPermissionsVM (..),
  UpdateUserProfileVM (..),
  UpdateUserSettingsVM (..),
  UserProfileVM (..),
  UserSettingsVM (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Swagger (ToSchema, declareNamedSchema)
import qualified Data.Swagger as Swagger
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data ApiKeyVM = ApiKeyVM
  { apiKeyVMValue :: Maybe Text -- ^ 
  , apiKeyVMDescription :: Maybe Text -- ^ 
  , apiKeyVMExpired :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiKeyVM")
instance ToJSON ApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiKeyVM")
instance ToSchema ApiKeyVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "apiKeyVM"


-- | 
data ApiKeysVM = ApiKeysVM
  { apiKeysVMApiKeys :: Maybe [ApiKeyVM] -- ^ 
  , apiKeysVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApiKeysVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiKeysVM")
instance ToJSON ApiKeysVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiKeysVM")
instance ToSchema ApiKeysVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "apiKeysVM"


-- | 
data BreadcrumbsModel = BreadcrumbsModel
  { breadcrumbsModelId :: Maybe Text -- ^ 
  , breadcrumbsModelName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BreadcrumbsModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "breadcrumbsModel")
instance ToJSON BreadcrumbsModel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "breadcrumbsModel")
instance ToSchema BreadcrumbsModel where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "breadcrumbsModel"


-- | 
data BreadcrumbsVM = BreadcrumbsVM
  { breadcrumbsVMBreadcrumbs :: Maybe [BreadcrumbsModel] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BreadcrumbsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "breadcrumbsVM")
instance ToJSON BreadcrumbsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "breadcrumbsVM")
instance ToSchema BreadcrumbsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "breadcrumbsVM"


-- | 
data CountVM = CountVM
  { countVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CountVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "countVM")
instance ToJSON CountVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "countVM")
instance ToSchema CountVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "countVM"


-- | 
data CreateApiKeyVM = CreateApiKeyVM
  { createApiKeyVMDescription :: Maybe Text -- ^ 
  , createApiKeyVMExpired :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createApiKeyVM")
instance ToJSON CreateApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createApiKeyVM")
instance ToSchema CreateApiKeyVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createApiKeyVM"


-- | 
data CreateDataSourceVM = CreateDataSourceVM
  { createDataSourceVMName :: Maybe Text -- ^ 
  , createDataSourceVMConnectionString :: Text -- ^ 
  , createDataSourceVMSubscriptionId :: Text -- ^ 
  , createDataSourceVMConnectionType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDataSourceVM")
instance ToJSON CreateDataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDataSourceVM")
instance ToSchema CreateDataSourceVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createDataSourceVM"


-- | 
data CreateGroupVM = CreateGroupVM
  { createGroupVMName :: Text -- ^ 
  , createGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupVM")
instance ToJSON CreateGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupVM")
instance ToSchema CreateGroupVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createGroupVM"


-- | 
data CreateSubscriptionInviteVM = CreateSubscriptionInviteVM
  { createSubscriptionInviteVMUsages :: Maybe Integer -- ^ 
  , createSubscriptionInviteVMDurable :: Maybe Bool -- ^ 
  , createSubscriptionInviteVMExpiredDate :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSubscriptionInviteVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSubscriptionInviteVM")
instance ToJSON CreateSubscriptionInviteVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSubscriptionInviteVM")
instance ToSchema CreateSubscriptionInviteVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createSubscriptionInviteVM"


-- | 
data DataSourcePermission = DataSourcePermission
  { dataSourcePermissionCreate :: Maybe Int -- ^ 
  , dataSourcePermissionDelete :: Maybe Int -- ^ 
  , dataSourcePermissionExecute :: Maybe Int -- ^ 
  , dataSourcePermissionGet :: Maybe Int -- ^ 
  , dataSourcePermissionUpdate :: Maybe Int -- ^ 
  , dataSourcePermissionAdministrate :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermission")
instance ToJSON DataSourcePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermission")
instance ToSchema DataSourcePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourcePermission"


-- | 
data DataSourcePermissions = DataSourcePermissions
  { dataSourcePermissionsOwnerId :: Maybe Text -- ^ 
  , dataSourcePermissionsOwner :: Maybe DataSourcePermission -- ^ 
  , dataSourcePermissionsGroups :: Maybe (Map.Map String DataSourcePermission) -- ^ 
  , dataSourcePermissionsOther :: Maybe DataSourcePermission -- ^ 
  , dataSourcePermissionsAnon :: Maybe DataSourcePermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermissions")
instance ToJSON DataSourcePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermissions")
instance ToSchema DataSourcePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourcePermissions"


-- | 
data DataSourcePermissionsVM = DataSourcePermissionsVM
  { dataSourcePermissionsVMPermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermissionsVM")
instance ToJSON DataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermissionsVM")
instance ToSchema DataSourcePermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourcePermissionsVM"


-- | 
data DataSourceVM = DataSourceVM
  { dataSourceVMId :: Maybe Text -- ^ 
  , dataSourceVMName :: Maybe Text -- ^ 
  , dataSourceVMConnectionType :: Maybe Text -- ^ 
  , dataSourceVMConnectionString :: Maybe Text -- ^ 
  , dataSourceVMDataStructure :: Maybe Text -- ^ 
  , dataSourceVMSubscriptionId :: Maybe Text -- ^ 
  , dataSourceVMEditedTime :: Maybe UTCTime -- ^ 
  , dataSourceVMEditorUserId :: Maybe Text -- ^ 
  , dataSourceVMCreatedTime :: Maybe UTCTime -- ^ 
  , dataSourceVMCreatorUserId :: Maybe Text -- ^ 
  , dataSourceVMIsConnected :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceVM")
instance ToJSON DataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceVM")
instance ToSchema DataSourceVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceVM"


-- | 
data DataSourcesVM = DataSourcesVM
  { dataSourcesVMDataSources :: Maybe [DataSourceVM] -- ^ 
  , dataSourcesVMCount :: Maybe Integer -- ^ 
  , dataSourcesVMSkip :: Maybe Int -- ^ 
  , dataSourcesVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcesVM")
instance ToJSON DataSourcesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcesVM")
instance ToSchema DataSourcesVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourcesVM"


-- | 
data DefaultPermissions = DefaultPermissions
  { defaultPermissionsFilePermissions :: Maybe FilePermissions -- ^ 
  , defaultPermissionsGroupPermissions :: Maybe GroupPermissions -- ^ 
  , defaultPermissionsDataSourcePermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DefaultPermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "defaultPermissions")
instance ToJSON DefaultPermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "defaultPermissions")
instance ToSchema DefaultPermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "defaultPermissions"


-- | 
data DefaultPermissionsVM = DefaultPermissionsVM
  { defaultPermissionsVMFilePermissions :: Maybe FilePermissions -- ^ 
  , defaultPermissionsVMDataSourcePermissions :: Maybe DataSourcePermissions -- ^ 
  , defaultPermissionsVMGroupPermissions :: Maybe GroupPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DefaultPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "defaultPermissionsVM")
instance ToJSON DefaultPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "defaultPermissionsVM")
instance ToSchema DefaultPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "defaultPermissionsVM"


-- | 
data DeleteApiKeyVM = DeleteApiKeyVM
  { deleteApiKeyVMApiKey :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deleteApiKeyVM")
instance ToJSON DeleteApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deleteApiKeyVM")
instance ToSchema DeleteApiKeyVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "deleteApiKeyVM"


-- | 
data ExportFolderCreateVM = ExportFolderCreateVM
  { exportFolderCreateVMName :: Maybe Text -- ^ 
  , exportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , exportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportFolderCreateVM")
instance ToJSON ExportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportFolderCreateVM")
instance ToSchema ExportFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportFolderCreateVM"


-- | 
data ExportReportTaskVM = ExportReportTaskVM
  { exportReportTaskVMFileName :: Maybe Text -- ^ 
  , exportReportTaskVMFolderId :: Maybe Text -- ^ 
  , exportReportTaskVMLocale :: Maybe Text -- ^ 
  , exportReportTaskVMPagesCount :: Maybe Int -- ^ 
  , exportReportTaskVMFormat :: Maybe Text -- ^ 
  , exportReportTaskVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportReportTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportReportTaskVM")
instance ToJSON ExportReportTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportReportTaskVM")
instance ToSchema ExportReportTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportReportTaskVM"


-- | 
data ExportTemplateTaskVM = ExportTemplateTaskVM
  { exportTemplateTaskVMFileName :: Maybe Text -- ^ 
  , exportTemplateTaskVMFolderId :: Maybe Text -- ^ 
  , exportTemplateTaskVMLocale :: Maybe Text -- ^ 
  , exportTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , exportTemplateTaskVMFormat :: Maybe Text -- ^ 
  , exportTemplateTaskVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  , exportTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportTemplateTaskVM")
instance ToJSON ExportTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportTemplateTaskVM")
instance ToSchema ExportTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportTemplateTaskVM"


-- | 
data ExportVM = ExportVM
  { exportVMFormat :: Maybe Text -- ^ 
  , exportVMReportId :: Maybe Text -- ^ 
  , exportVMName :: Maybe Text -- ^ 
  , exportVMParentId :: Maybe Text -- ^ 
  , exportVMTags :: Maybe [Text] -- ^ 
  , exportVMIcon :: Maybe Text -- ^ 
  , exportVMType :: Maybe Text -- ^ 
  , exportVMSize :: Maybe Integer -- ^ 
  , exportVMSubscriptionId :: Maybe Text -- ^ 
  , exportVMStatus :: Maybe Text -- ^ 
  , exportVMStatusReason :: Maybe Text -- ^ 
  , exportVMId :: Maybe Text -- ^ 
  , exportVMCreatedTime :: Maybe UTCTime -- ^ 
  , exportVMCreatorUserId :: Maybe Text -- ^ 
  , exportVMEditedTime :: Maybe UTCTime -- ^ 
  , exportVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportVM")
instance ToJSON ExportVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportVM")
instance ToSchema ExportVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportVM"


-- | 
data ExportsVM = ExportsVM
  { exportsVMFiles :: Maybe [ExportVM] -- ^ 
  , exportsVMCount :: Maybe Integer -- ^ 
  , exportsVMSkip :: Maybe Int -- ^ 
  , exportsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportsVM")
instance ToJSON ExportsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportsVM")
instance ToSchema ExportsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportsVM"


-- | 
data FileIconVM = FileIconVM
  { fileIconVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileIconVM")
instance ToJSON FileIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileIconVM")
instance ToSchema FileIconVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileIconVM"


-- | 
data FilePermission = FilePermission
  { filePermissionCreate :: Maybe Int -- ^ 
  , filePermissionDelete :: Maybe Int -- ^ 
  , filePermissionExecute :: Maybe Int -- ^ 
  , filePermissionGet :: Maybe Int -- ^ 
  , filePermissionUpdate :: Maybe Int -- ^ 
  , filePermissionAdministrate :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filePermission")
instance ToJSON FilePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filePermission")
instance ToSchema FilePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "filePermission"


-- | 
data FilePermissions = FilePermissions
  { filePermissionsOwnerId :: Maybe Text -- ^ 
  , filePermissionsOwner :: Maybe FilePermission -- ^ 
  , filePermissionsGroups :: Maybe (Map.Map String FilePermission) -- ^ 
  , filePermissionsOther :: Maybe FilePermission -- ^ 
  , filePermissionsAnon :: Maybe FilePermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filePermissions")
instance ToJSON FilePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filePermissions")
instance ToSchema FilePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "filePermissions"


-- | 
data FilePermissionsVM = FilePermissionsVM
  { filePermissionsVMPermissions :: Maybe FilePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filePermissionsVM")
instance ToJSON FilePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filePermissionsVM")
instance ToSchema FilePermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "filePermissionsVM"


-- | 
data FileRenameVM = FileRenameVM
  { fileRenameVMName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileRenameVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileRenameVM")
instance ToJSON FileRenameVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileRenameVM")
instance ToSchema FileRenameVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileRenameVM"


-- | 
data FileTagsUpdateVM = FileTagsUpdateVM
  { fileTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileTagsUpdateVM")
instance ToJSON FileTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileTagsUpdateVM")
instance ToSchema FileTagsUpdateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileTagsUpdateVM"


-- | 
data FileVM = FileVM
  { fileVMName :: Maybe Text -- ^ 
  , fileVMParentId :: Maybe Text -- ^ 
  , fileVMTags :: Maybe [Text] -- ^ 
  , fileVMIcon :: Maybe Text -- ^ 
  , fileVMType :: Maybe Text -- ^ 
  , fileVMSize :: Maybe Integer -- ^ 
  , fileVMSubscriptionId :: Maybe Text -- ^ 
  , fileVMStatus :: Maybe Text -- ^ 
  , fileVMStatusReason :: Maybe Text -- ^ 
  , fileVMId :: Maybe Text -- ^ 
  , fileVMCreatedTime :: Maybe UTCTime -- ^ 
  , fileVMCreatorUserId :: Maybe Text -- ^ 
  , fileVMEditedTime :: Maybe UTCTime -- ^ 
  , fileVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileVM")
instance ToJSON FileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileVM")
instance ToSchema FileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileVM"


-- | 
data FilesVM = FilesVM
  { filesVMFiles :: Maybe [FileVM] -- ^ 
  , filesVMCount :: Maybe Integer -- ^ 
  , filesVMSkip :: Maybe Int -- ^ 
  , filesVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filesVM")
instance ToJSON FilesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filesVM")
instance ToSchema FilesVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "filesVM"


-- | 
data FolderIconVM = FolderIconVM
  { folderIconVMIcon :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderIconVM")
instance ToJSON FolderIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderIconVM")
instance ToSchema FolderIconVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "folderIconVM"


-- | 
data FolderRenameVM = FolderRenameVM
  { folderRenameVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderRenameVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderRenameVM")
instance ToJSON FolderRenameVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderRenameVM")
instance ToSchema FolderRenameVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "folderRenameVM"


-- | 
data FolderTagsUpdateVM = FolderTagsUpdateVM
  { folderTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderTagsUpdateVM")
instance ToJSON FolderTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderTagsUpdateVM")
instance ToSchema FolderTagsUpdateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "folderTagsUpdateVM"


-- | 
data GroupPermission = GroupPermission
  { groupPermissionCreate :: Maybe Int -- ^ 
  , groupPermissionDelete :: Maybe Int -- ^ 
  , groupPermissionExecute :: Maybe Int -- ^ 
  , groupPermissionGet :: Maybe Int -- ^ 
  , groupPermissionUpdate :: Maybe Int -- ^ 
  , groupPermissionAdministrate :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermission")
instance ToJSON GroupPermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermission")
instance ToSchema GroupPermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupPermission"


-- | 
data GroupPermissions = GroupPermissions
  { groupPermissionsOwnerId :: Maybe Text -- ^ 
  , groupPermissionsOwner :: Maybe GroupPermission -- ^ 
  , groupPermissionsGroups :: Maybe (Map.Map String GroupPermission) -- ^ 
  , groupPermissionsOther :: Maybe GroupPermission -- ^ 
  , groupPermissionsAnon :: Maybe GroupPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermissions")
instance ToJSON GroupPermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermissions")
instance ToSchema GroupPermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupPermissions"


-- | 
data GroupPermissionsVM = GroupPermissionsVM
  { groupPermissionsVMPermissions :: Maybe GroupPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermissionsVM")
instance ToJSON GroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermissionsVM")
instance ToSchema GroupPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupPermissionsVM"


-- | 
data GroupUserVM = GroupUserVM
  { groupUserVMUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupUserVM")
instance ToJSON GroupUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupUserVM")
instance ToSchema GroupUserVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupUserVM"


-- | 
data GroupUsersVM = GroupUsersVM
  { groupUsersVMUsers :: Maybe [GroupUserVM] -- ^ 
  , groupUsersVMCount :: Maybe Integer -- ^ 
  , groupUsersVMTake :: Maybe Int -- ^ 
  , groupUsersVMSkip :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUsersVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupUsersVM")
instance ToJSON GroupUsersVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupUsersVM")
instance ToSchema GroupUsersVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupUsersVM"


-- | 
data GroupVM = GroupVM
  { groupVMId :: Maybe Text -- ^ 
  , groupVMName :: Maybe Text -- ^ 
  , groupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupVM")
instance ToJSON GroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupVM")
instance ToSchema GroupVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupVM"


-- | 
data GroupsVM = GroupsVM
  { groupsVMGroups :: Maybe [GroupVM] -- ^ 
  , groupsVMCount :: Maybe Integer -- ^ 
  , groupsVMSkip :: Maybe Int -- ^ 
  , groupsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupsVM")
instance ToJSON GroupsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupsVM")
instance ToSchema GroupsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupsVM"


-- | 
data InvitedUser = InvitedUser
  { invitedUserUserId :: Maybe Text -- ^ 
  , invitedUserInvitedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InvitedUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "invitedUser")
instance ToJSON InvitedUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "invitedUser")
instance ToSchema InvitedUser where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "invitedUser"


-- | 
data PrepareTemplateTaskVM = PrepareTemplateTaskVM
  { prepareTemplateTaskVMName :: Maybe Text -- ^ 
  , prepareTemplateTaskVMLocale :: Maybe Text -- ^ 
  , prepareTemplateTaskVMParentFolderId :: Maybe Text -- ^ 
  , prepareTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , prepareTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PrepareTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "prepareTemplateTaskVM")
instance ToJSON PrepareTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "prepareTemplateTaskVM")
instance ToSchema PrepareTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "prepareTemplateTaskVM"


-- | 
newtype ProblemDetails = ProblemDetails { unProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data RenameDataSourceVM = RenameDataSourceVM
  { renameDataSourceVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameDataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameDataSourceVM")
instance ToJSON RenameDataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameDataSourceVM")
instance ToSchema RenameDataSourceVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "renameDataSourceVM"


-- | 
data RenameGroupVM = RenameGroupVM
  { renameGroupVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameGroupVM")
instance ToJSON RenameGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameGroupVM")
instance ToSchema RenameGroupVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "renameGroupVM"


-- | 
data RenameSubscriptionVM = RenameSubscriptionVM
  { renameSubscriptionVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameSubscriptionVM")
instance ToJSON RenameSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameSubscriptionVM")
instance ToSchema RenameSubscriptionVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "renameSubscriptionVM"


-- | 
data ReportCreateVM = ReportCreateVM
  { reportCreateVMTemplateId :: Maybe Text -- ^ 
  , reportCreateVMReportInfo :: Maybe ReportInfo -- ^ 
  , reportCreateVMName :: Maybe Text -- ^ 
  , reportCreateVMTags :: Maybe [Text] -- ^ 
  , reportCreateVMIcon :: Maybe Text -- ^ 
  , reportCreateVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportCreateVM")
instance ToJSON ReportCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportCreateVM")
instance ToSchema ReportCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportCreateVM"


-- | 
data ReportFolderCreateVM = ReportFolderCreateVM
  { reportFolderCreateVMName :: Maybe Text -- ^ 
  , reportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , reportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportFolderCreateVM")
instance ToJSON ReportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportFolderCreateVM")
instance ToSchema ReportFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportFolderCreateVM"


-- | 
data ReportInfo = ReportInfo
  { reportInfoAuthor :: Maybe Text -- ^ 
  , reportInfoCreated :: Maybe UTCTime -- ^ 
  , reportInfoCreatorVersion :: Maybe Text -- ^ 
  , reportInfoDescription :: Maybe Text -- ^ 
  , reportInfoModified :: Maybe UTCTime -- ^ 
  , reportInfoName :: Maybe Text -- ^ 
  , reportInfoPicture :: Maybe Text -- ^ 
  , reportInfoPreviewPictureRatio :: Maybe Float -- ^ 
  , reportInfoSaveMode :: Maybe Text -- ^ 
  , reportInfoSavePreviewPicture :: Maybe Bool -- ^ 
  , reportInfoTag :: Maybe Text -- ^ 
  , reportInfoVersion :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportInfo")
instance ToJSON ReportInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportInfo")
instance ToSchema ReportInfo where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportInfo"


-- | 
data ReportVM = ReportVM
  { reportVMTemplateId :: Maybe Text -- ^ 
  , reportVMReportInfo :: Maybe ReportInfo -- ^ 
  , reportVMName :: Maybe Text -- ^ 
  , reportVMParentId :: Maybe Text -- ^ 
  , reportVMTags :: Maybe [Text] -- ^ 
  , reportVMIcon :: Maybe Text -- ^ 
  , reportVMType :: Maybe Text -- ^ 
  , reportVMSize :: Maybe Integer -- ^ 
  , reportVMSubscriptionId :: Maybe Text -- ^ 
  , reportVMStatus :: Maybe Text -- ^ 
  , reportVMStatusReason :: Maybe Text -- ^ 
  , reportVMId :: Maybe Text -- ^ 
  , reportVMCreatedTime :: Maybe UTCTime -- ^ 
  , reportVMCreatorUserId :: Maybe Text -- ^ 
  , reportVMEditedTime :: Maybe UTCTime -- ^ 
  , reportVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportVM")
instance ToJSON ReportVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportVM")
instance ToSchema ReportVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportVM"


-- | 
data ReportsVM = ReportsVM
  { reportsVMFiles :: Maybe [ReportVM] -- ^ 
  , reportsVMCount :: Maybe Integer -- ^ 
  , reportsVMSkip :: Maybe Int -- ^ 
  , reportsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportsVM")
instance ToJSON ReportsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportsVM")
instance ToSchema ReportsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportsVM"


-- | 
data SubscriptionFolder = SubscriptionFolder
  { subscriptionFolderFolderId :: Maybe Text -- ^ 
  , subscriptionFolderBytesUsed :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionFolder where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionFolder")
instance ToJSON SubscriptionFolder where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionFolder")
instance ToSchema SubscriptionFolder where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionFolder"


-- | 
data SubscriptionInviteVM = SubscriptionInviteVM
  { subscriptionInviteVMUsages :: Maybe Integer -- ^ 
  , subscriptionInviteVMDurable :: Maybe Bool -- ^ 
  , subscriptionInviteVMAccessToken :: Maybe Text -- ^ 
  , subscriptionInviteVMExpiredDate :: Maybe UTCTime -- ^ 
  , subscriptionInviteVMAddedUsers :: Maybe [InvitedUser] -- ^ 
  , subscriptionInviteVMCreatorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionInviteVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionInviteVM")
instance ToJSON SubscriptionInviteVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionInviteVM")
instance ToSchema SubscriptionInviteVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionInviteVM"


-- | 
data SubscriptionInvitesVM = SubscriptionInvitesVM
  { subscriptionInvitesVMInvites :: Maybe [SubscriptionInviteVM] -- ^ 
  , subscriptionInvitesVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionInvitesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionInvitesVM")
instance ToJSON SubscriptionInvitesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionInvitesVM")
instance ToSchema SubscriptionInvitesVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionInvitesVM"


-- | 
data SubscriptionPeriodVM = SubscriptionPeriodVM
  { subscriptionPeriodVMStartTime :: Maybe UTCTime -- ^ 
  , subscriptionPeriodVMEndTime :: Maybe UTCTime -- ^ 
  , subscriptionPeriodVMPlan :: Maybe SubscriptionPlanVM -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPeriodVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPeriodVM")
instance ToJSON SubscriptionPeriodVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPeriodVM")
instance ToSchema SubscriptionPeriodVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPeriodVM"


-- | 
data SubscriptionPermission = SubscriptionPermission
  { subscriptionPermissionCreate :: Maybe Int -- ^ 
  , subscriptionPermissionDelete :: Maybe Int -- ^ 
  , subscriptionPermissionExecute :: Maybe Int -- ^ 
  , subscriptionPermissionGet :: Maybe Int -- ^ 
  , subscriptionPermissionUpdate :: Maybe Int -- ^ 
  , subscriptionPermissionAdministrate :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPermission")
instance ToJSON SubscriptionPermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPermission")
instance ToSchema SubscriptionPermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPermission"


-- | 
data SubscriptionPermissions = SubscriptionPermissions
  { subscriptionPermissionsOwnerId :: Maybe Text -- ^ 
  , subscriptionPermissionsOwner :: Maybe SubscriptionPermission -- ^ 
  , subscriptionPermissionsGroups :: Maybe (Map.Map String SubscriptionPermission) -- ^ 
  , subscriptionPermissionsOther :: Maybe SubscriptionPermission -- ^ 
  , subscriptionPermissionsAnon :: Maybe SubscriptionPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPermissions")
instance ToJSON SubscriptionPermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPermissions")
instance ToSchema SubscriptionPermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPermissions"


-- | 
data SubscriptionPermissionsVM = SubscriptionPermissionsVM
  { subscriptionPermissionsVMPermissions :: Maybe SubscriptionPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPermissionsVM")
instance ToJSON SubscriptionPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPermissionsVM")
instance ToSchema SubscriptionPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPermissionsVM"


-- | 
data SubscriptionPlanVM = SubscriptionPlanVM
  { subscriptionPlanVMId :: Maybe Text -- ^ 
  , subscriptionPlanVMIsActive :: Maybe Bool -- ^ 
  , subscriptionPlanVMDisplayName :: Maybe Text -- ^ 
  , subscriptionPlanVMTimePeriodType :: Maybe Text -- ^ 
  , subscriptionPlanVMTimePeriod :: Maybe Int -- ^ 
  , subscriptionPlanVMReadonlyTimeLimitType :: Maybe Text -- ^ 
  , subscriptionPlanVMReadonlyTimeLimit :: Maybe Int -- ^ 
  , subscriptionPlanVMTemplatesSpaceLimit :: Maybe Integer -- ^ 
  , subscriptionPlanVMReportsSpaceLimit :: Maybe Integer -- ^ 
  , subscriptionPlanVMExportsSpaceLimit :: Maybe Integer -- ^ 
  , subscriptionPlanVMFileUploadSizeLimit :: Maybe Integer -- ^ 
  , subscriptionPlanVMDataSourceLimit :: Maybe Int -- ^ 
  , subscriptionPlanVMMaxUsersCount :: Maybe Int -- ^ 
  , subscriptionPlanVMHasSpaceOverdraft :: Maybe Bool -- ^ 
  , subscriptionPlanVMGroupLimit :: Maybe Int -- ^ 
  , subscriptionPlanVMOnlineDesigner :: Maybe Bool -- ^ 
  , subscriptionPlanVMIsDemo :: Maybe Bool -- ^ 
  , subscriptionPlanVMUrlToBuy :: Maybe Text -- ^ 
  , subscriptionPlanVMUnlimitedPage :: Maybe Bool -- ^ 
  , subscriptionPlanVMPageLimit :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPlanVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPlanVM")
instance ToJSON SubscriptionPlanVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPlanVM")
instance ToSchema SubscriptionPlanVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPlanVM"


-- | 
data SubscriptionPlansVM = SubscriptionPlansVM
  { subscriptionPlansVMSubscriptionPlans :: Maybe [SubscriptionPlanVM] -- ^ 
  , subscriptionPlansVMCount :: Maybe Integer -- ^ 
  , subscriptionPlansVMSkip :: Maybe Int -- ^ 
  , subscriptionPlansVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPlansVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPlansVM")
instance ToJSON SubscriptionPlansVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPlansVM")
instance ToSchema SubscriptionPlansVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPlansVM"


-- | 
data SubscriptionUserVM = SubscriptionUserVM
  { subscriptionUserVMUserId :: Maybe Text -- ^ 
  , subscriptionUserVMGroups :: Maybe [GroupVM] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionUserVM")
instance ToJSON SubscriptionUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionUserVM")
instance ToSchema SubscriptionUserVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionUserVM"


-- | 
data SubscriptionUsersVM = SubscriptionUsersVM
  { subscriptionUsersVMUsers :: Maybe [SubscriptionUserVM] -- ^ 
  , subscriptionUsersVMCount :: Maybe Integer -- ^ 
  , subscriptionUsersVMTake :: Maybe Int -- ^ 
  , subscriptionUsersVMSkip :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUsersVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionUsersVM")
instance ToJSON SubscriptionUsersVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionUsersVM")
instance ToSchema SubscriptionUsersVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionUsersVM"


-- | 
data SubscriptionVM = SubscriptionVM
  { subscriptionVMId :: Maybe Text -- ^ 
  , subscriptionVMName :: Maybe Text -- ^ 
  , subscriptionVMLocale :: Maybe Text -- ^ 
  , subscriptionVMCurrent :: Maybe SubscriptionPeriodVM -- ^ 
  , subscriptionVMOld :: Maybe [SubscriptionPeriodVM] -- ^ 
  , subscriptionVMTemplatesFolder :: Maybe SubscriptionFolder -- ^ 
  , subscriptionVMReportsFolder :: Maybe SubscriptionFolder -- ^ 
  , subscriptionVMExportsFolder :: Maybe SubscriptionFolder -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionVM")
instance ToJSON SubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionVM")
instance ToSchema SubscriptionVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionVM"


-- | 
data SubscriptionsVM = SubscriptionsVM
  { subscriptionsVMSubscriptions :: Maybe [SubscriptionVM] -- ^ 
  , subscriptionsVMCount :: Maybe Integer -- ^ 
  , subscriptionsVMSkip :: Maybe Int -- ^ 
  , subscriptionsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionsVM")
instance ToJSON SubscriptionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionsVM")
instance ToSchema SubscriptionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionsVM"


-- | 
data TemplateCreateVM = TemplateCreateVM
  { templateCreateVMName :: Maybe Text -- ^ 
  , templateCreateVMTags :: Maybe [Text] -- ^ 
  , templateCreateVMIcon :: Maybe Text -- ^ 
  , templateCreateVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateCreateVM")
instance ToJSON TemplateCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateCreateVM")
instance ToSchema TemplateCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templateCreateVM"


-- | 
data TemplateFolderCreateVM = TemplateFolderCreateVM
  { templateFolderCreateVMName :: Maybe Text -- ^ 
  , templateFolderCreateVMTags :: Maybe [Text] -- ^ 
  , templateFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateFolderCreateVM")
instance ToJSON TemplateFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateFolderCreateVM")
instance ToSchema TemplateFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templateFolderCreateVM"


-- | 
data TemplateVM = TemplateVM
  { templateVMReportInfo :: Maybe ReportInfo -- ^ 
  , templateVMName :: Maybe Text -- ^ 
  , templateVMParentId :: Maybe Text -- ^ 
  , templateVMTags :: Maybe [Text] -- ^ 
  , templateVMIcon :: Maybe Text -- ^ 
  , templateVMType :: Maybe Text -- ^ 
  , templateVMSize :: Maybe Integer -- ^ 
  , templateVMSubscriptionId :: Maybe Text -- ^ 
  , templateVMStatus :: Maybe Text -- ^ 
  , templateVMStatusReason :: Maybe Text -- ^ 
  , templateVMId :: Maybe Text -- ^ 
  , templateVMCreatedTime :: Maybe UTCTime -- ^ 
  , templateVMCreatorUserId :: Maybe Text -- ^ 
  , templateVMEditedTime :: Maybe UTCTime -- ^ 
  , templateVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateVM")
instance ToJSON TemplateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateVM")
instance ToSchema TemplateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templateVM"


-- | 
data TemplatesVM = TemplatesVM
  { templatesVMFiles :: Maybe [TemplateVM] -- ^ 
  , templatesVMCount :: Maybe Integer -- ^ 
  , templatesVMSkip :: Maybe Int -- ^ 
  , templatesVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplatesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templatesVM")
instance ToJSON TemplatesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templatesVM")
instance ToSchema TemplatesVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templatesVM"


-- | 
data UpdateDataSourceConnectionStringVM = UpdateDataSourceConnectionStringVM
  { updateDataSourceConnectionStringVMConnectionString :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceConnectionStringVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceConnectionStringVM")
instance ToJSON UpdateDataSourceConnectionStringVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceConnectionStringVM")
instance ToSchema UpdateDataSourceConnectionStringVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateDataSourceConnectionStringVM"


-- | 
data UpdateDataSourcePermissionsVM = UpdateDataSourcePermissionsVM
  { updateDataSourcePermissionsVMNewPermissions :: DataSourcePermissions -- ^ 
  , updateDataSourcePermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourcePermissionsVM")
instance ToJSON UpdateDataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourcePermissionsVM")
instance ToSchema UpdateDataSourcePermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateDataSourcePermissionsVM"


-- | 
data UpdateDataSourceSubscriptionVM = UpdateDataSourceSubscriptionVM
  { updateDataSourceSubscriptionVMSubscriptionId :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceSubscriptionVM")
instance ToJSON UpdateDataSourceSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceSubscriptionVM")
instance ToSchema UpdateDataSourceSubscriptionVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateDataSourceSubscriptionVM"


-- | 
data UpdateDefaultPermissionsVM = UpdateDefaultPermissionsVM
  { updateDefaultPermissionsVMFilePermissions :: Maybe UpdateFilePermissionsVM -- ^ 
  , updateDefaultPermissionsVMGroupPermissions :: Maybe UpdateGroupPermissionsVM -- ^ 
  , updateDefaultPermissionsVMDataSourcePermissions :: Maybe UpdateDataSourcePermissionsVM -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDefaultPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDefaultPermissionsVM")
instance ToJSON UpdateDefaultPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDefaultPermissionsVM")
instance ToSchema UpdateDefaultPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateDefaultPermissionsVM"


-- | 
data UpdateFilePermissionsVM = UpdateFilePermissionsVM
  { updateFilePermissionsVMNewPermissions :: FilePermissions -- ^ 
  , updateFilePermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateFilePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateFilePermissionsVM")
instance ToJSON UpdateFilePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateFilePermissionsVM")
instance ToSchema UpdateFilePermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateFilePermissionsVM"


-- | 
data UpdateGroupPermissionsVM = UpdateGroupPermissionsVM
  { updateGroupPermissionsVMNewPermissions :: GroupPermissions -- ^ 
  , updateGroupPermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupPermissionsVM")
instance ToJSON UpdateGroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupPermissionsVM")
instance ToSchema UpdateGroupPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateGroupPermissionsVM"


-- | 
data UpdateSubscriptionLocaleVM = UpdateSubscriptionLocaleVM
  { updateSubscriptionLocaleVMLocale :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionLocaleVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionLocaleVM")
instance ToJSON UpdateSubscriptionLocaleVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionLocaleVM")
instance ToSchema UpdateSubscriptionLocaleVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateSubscriptionLocaleVM"


-- | 
data UpdateSubscriptionPermissionsVM = UpdateSubscriptionPermissionsVM
  { updateSubscriptionPermissionsVMNewPermissions :: SubscriptionPermissions -- ^ 
  , updateSubscriptionPermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionPermissionsVM")
instance ToJSON UpdateSubscriptionPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionPermissionsVM")
instance ToSchema UpdateSubscriptionPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateSubscriptionPermissionsVM"


-- | 
data UpdateUserProfileVM = UpdateUserProfileVM
  { updateUserProfileVMName :: Maybe Text -- ^ 
  , updateUserProfileVMUsername :: Maybe Text -- ^ 
  , updateUserProfileVMEmail :: Maybe Text -- ^ 
  , updateUserProfileVMPasswordNew :: Maybe Text -- ^ 
  , updateUserProfileVMPasswordNew2 :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateUserProfileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUserProfileVM")
instance ToJSON UpdateUserProfileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUserProfileVM")
instance ToSchema UpdateUserProfileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateUserProfileVM"


-- | 
data UpdateUserSettingsVM = UpdateUserSettingsVM
  { updateUserSettingsVMProfileVisibility :: Maybe Int -- ^ 
  , updateUserSettingsVMDefaultSubscription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateUserSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUserSettingsVM")
instance ToJSON UpdateUserSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUserSettingsVM")
instance ToSchema UpdateUserSettingsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "updateUserSettingsVM"


-- | 
data UserProfileVM = UserProfileVM
  { userProfileVMId :: Maybe Text -- ^ 
  , userProfileVMName :: Maybe Text -- ^ 
  , userProfileVMUsername :: Maybe Text -- ^ 
  , userProfileVMEmail :: Maybe Text -- ^ 
  , userProfileVMIsReadOnly :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserProfileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userProfileVM")
instance ToJSON UserProfileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userProfileVM")
instance ToSchema UserProfileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "userProfileVM"


-- | 
data UserSettingsVM = UserSettingsVM
  { userSettingsVMProfileVisibility :: Maybe Int -- ^ 
  , userSettingsVMDefaultSubscription :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userSettingsVM")
instance ToJSON UserSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userSettingsVM")
instance ToSchema UserSettingsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "userSettingsVM"


uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do viceversa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("~=", "'Tilde_Equal")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
