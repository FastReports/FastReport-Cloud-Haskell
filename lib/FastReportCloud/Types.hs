{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module FastReportCloud.Types (
  AdminExportFolderCreateVM (..),
  AdminFolderCreateVM (..),
  AdminReportFolderCreateVM (..),
  AdminSubscriptionVM (..),
  AdminTemplateFolderCreateVM (..),
  ApiKeyVM (..),
  ApiKeysVM (..),
  AppMixins (..),
  BreadcrumbsModel (..),
  BreadcrumbsVM (..),
  CountVM (..),
  CreateApiKeyVM (..),
  CreateDataSourceAdminVM (..),
  CreateDataSourceVM (..),
  CreateEmailTaskVM (..),
  CreateEndpointVM (..),
  CreateExportReportTaskVM (..),
  CreateExportTemplateTaskVM (..),
  CreateFetchTaskVM (..),
  CreateGroupAdminVM (..),
  CreateGroupVM (..),
  CreatePrepareTemplateTaskVM (..),
  CreateSubscriptionInviteVM (..),
  CreateTaskBaseVM (..),
  CreateTransformTaskBaseVM (..),
  CreateTransportTaskBaseVM (..),
  CreateWebhookTaskVM (..),
  DataSourceAdministrate (..),
  DataSourceConnectionType (..),
  DataSourceCreate (..),
  DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission (..),
  DataSourceDelete (..),
  DataSourceExecute (..),
  DataSourceGet (..),
  DataSourcePermission (..),
  DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions (..),
  DataSourcePermissions (..),
  DataSourcePermissionsVM (..),
  DataSourceSorting (..),
  DataSourceStatus (..),
  DataSourceUpdate (..),
  DataSourceVM (..),
  DataSourcesVM (..),
  DefaultPermissions (..),
  DefaultPermissionsVM (..),
  DeleteApiKeyVM (..),
  EmailTaskVM (..),
  EndpointVM (..),
  EntityVM (..),
  ExportCreateAdminVM (..),
  ExportCreateVM (..),
  ExportFolderCreateVM (..),
  ExportFormat (..),
  ExportReportTaskVM (..),
  ExportReportVM (..),
  ExportTemplateTaskVM (..),
  ExportTemplateVM (..),
  ExportVM (..),
  ExportVMFilesVMBase (..),
  ExportsVM (..),
  FetchTaskVM (..),
  FileAdministrate (..),
  FileCreate (..),
  FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission (..),
  FileCreateVM (..),
  FileDelete (..),
  FileExecute (..),
  FileGet (..),
  FileIconVM (..),
  FileKind (..),
  FilePermission (..),
  FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions (..),
  FilePermissions (..),
  FilePermissionsVM (..),
  FileRenameVM (..),
  FileSorting (..),
  FileStatus (..),
  FileStatusReason (..),
  FileTagsUpdateVM (..),
  FileType (..),
  FileUpdate (..),
  FileVM (..),
  FileVMFilesVMBase (..),
  FilesVM (..),
  FolderCreateVM (..),
  FolderIconVM (..),
  FolderRenameVM (..),
  FolderTagsUpdateVM (..),
  GroupAdministrate (..),
  GroupCreate (..),
  GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission (..),
  GroupDelete (..),
  GroupExecute (..),
  GroupGet (..),
  GroupPermission (..),
  GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions (..),
  GroupPermissions (..),
  GroupPermissionsVM (..),
  GroupUpdate (..),
  GroupUserVM (..),
  GroupUsersVM (..),
  GroupVM (..),
  GroupsVM (..),
  InputFileVM (..),
  InvitedUser (..),
  MyPermissionsVM (..),
  OutputFileVM (..),
  PrepareTemplateTaskVM (..),
  PrepareTemplateVM (..),
  ProblemDetails (..),
  ProfileVisibility (..),
  RenameDataSourceVM (..),
  RenameGroupVM (..),
  RenameSubscriptionVM (..),
  ReportCreateAdminVM (..),
  ReportCreateVM (..),
  ReportFolderCreateVM (..),
  ReportInfo (..),
  ReportVM (..),
  ReportVMFilesVMBase (..),
  ReportsVM (..),
  RunEmailTaskVM (..),
  RunEndpointVM (..),
  RunExportReportTaskVM (..),
  RunExportTemplateTaskVM (..),
  RunFetchTaskVM (..),
  RunInputFileVM (..),
  RunPrepareTemplateTaskVM (..),
  RunTaskBaseVM (..),
  RunTransformTaskBaseVM (..),
  RunTransportTaskBaseVM (..),
  RunWebhookTaskVM (..),
  SaveMode (..),
  ServerConfigurationVM (..),
  SubscriptionAdministrate (..),
  SubscriptionCreate (..),
  SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission (..),
  SubscriptionDelete (..),
  SubscriptionExecute (..),
  SubscriptionFolder (..),
  SubscriptionGet (..),
  SubscriptionInviteVM (..),
  SubscriptionInvitesVM (..),
  SubscriptionPeriodVM (..),
  SubscriptionPermission (..),
  SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions (..),
  SubscriptionPermissions (..),
  SubscriptionPermissionsVM (..),
  SubscriptionPlanVM (..),
  SubscriptionPlansVM (..),
  SubscriptionUpdate (..),
  SubscriptionUserVM (..),
  SubscriptionUsersVM (..),
  SubscriptionVM (..),
  SubscriptionsVM (..),
  TaskBaseVM (..),
  TaskSettingsVM (..),
  TaskType (..),
  TasksVM (..),
  TemplateCreateAdminVM (..),
  TemplateCreateVM (..),
  TemplateFolderCreateVM (..),
  TemplateVM (..),
  TemplateVMFilesVMBase (..),
  TemplatesVM (..),
  TimePeriodType (..),
  TransformTaskBaseVM (..),
  TransportTaskBaseVM (..),
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
  ValidationProblemDetails (..),
  WebhookTaskVM (..),
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
data AdminExportFolderCreateVM = AdminExportFolderCreateVM
  { adminExportFolderCreateVMName :: Maybe Text -- ^ 
  , adminExportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminExportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminExportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminExportFolderCreateVM")
instance ToJSON AdminExportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminExportFolderCreateVM")
instance ToSchema AdminExportFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "adminExportFolderCreateVM"


-- | 
data AdminFolderCreateVM = AdminFolderCreateVM
  { adminFolderCreateVMParentId :: Text -- ^ 
  , adminFolderCreateVMOwnerId :: Text -- ^ 
  , adminFolderCreateVMName :: Maybe Text -- ^ 
  , adminFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminFolderCreateVM")
instance ToJSON AdminFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminFolderCreateVM")
instance ToSchema AdminFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "adminFolderCreateVM"


-- | 
data AdminReportFolderCreateVM = AdminReportFolderCreateVM
  { adminReportFolderCreateVMName :: Maybe Text -- ^ 
  , adminReportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminReportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminReportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminReportFolderCreateVM")
instance ToJSON AdminReportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminReportFolderCreateVM")
instance ToSchema AdminReportFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "adminReportFolderCreateVM"


-- | 
data AdminSubscriptionVM = AdminSubscriptionVM
  { adminSubscriptionVMDefaultPermissions :: Maybe DefaultPermissions -- ^ 
  , adminSubscriptionVMId :: Maybe Text -- ^ 
  , adminSubscriptionVMName :: Maybe Text -- ^ 
  , adminSubscriptionVMLocale :: Maybe Text -- ^ 
  , adminSubscriptionVMCurrent :: Maybe SubscriptionPeriodVM -- ^ 
  , adminSubscriptionVMOld :: Maybe [SubscriptionPeriodVM] -- ^ 
  , adminSubscriptionVMTemplatesFolder :: Maybe SubscriptionFolder -- ^ 
  , adminSubscriptionVMReportsFolder :: Maybe SubscriptionFolder -- ^ 
  , adminSubscriptionVMExportsFolder :: Maybe SubscriptionFolder -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminSubscriptionVM")
instance ToJSON AdminSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminSubscriptionVM")
instance ToSchema AdminSubscriptionVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "adminSubscriptionVM"


-- | 
data AdminTemplateFolderCreateVM = AdminTemplateFolderCreateVM
  { adminTemplateFolderCreateVMName :: Maybe Text -- ^ 
  , adminTemplateFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminTemplateFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminTemplateFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminTemplateFolderCreateVM")
instance ToJSON AdminTemplateFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminTemplateFolderCreateVM")
instance ToSchema AdminTemplateFolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "adminTemplateFolderCreateVM"


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
data AppMixins = AppMixins
  { appMixinsHead :: Maybe Text -- ^ 
  , appMixinsBody :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AppMixins where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appMixins")
instance ToJSON AppMixins where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appMixins")
instance ToSchema AppMixins where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "appMixins"


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
data CreateDataSourceAdminVM = CreateDataSourceAdminVM
  { createDataSourceAdminVMOwnerId :: Text -- ^ 
  , createDataSourceAdminVMName :: Maybe Text -- ^ 
  , createDataSourceAdminVMConnectionString :: Text -- ^ 
  , createDataSourceAdminVMSubscriptionId :: Text -- ^ 
  , createDataSourceAdminVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDataSourceAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDataSourceAdminVM")
instance ToJSON CreateDataSourceAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDataSourceAdminVM")
instance ToSchema CreateDataSourceAdminVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createDataSourceAdminVM"


-- | 
data CreateDataSourceVM = CreateDataSourceVM
  { createDataSourceVMName :: Maybe Text -- ^ 
  , createDataSourceVMConnectionString :: Text -- ^ 
  , createDataSourceVMSubscriptionId :: Text -- ^ 
  , createDataSourceVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
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
data CreateEmailTaskVM = CreateEmailTaskVM
  { createEmailTaskVMPassword :: Maybe Text -- ^ 
  , createEmailTaskVMBody :: Maybe Text -- ^ 
  , createEmailTaskVMIsBodyHtml :: Maybe Bool -- ^ 
  , createEmailTaskVMSubject :: Maybe Text -- ^ 
  , createEmailTaskVMTo :: Maybe [Text] -- ^ 
  , createEmailTaskVMFrom :: Maybe Text -- ^ 
  , createEmailTaskVMUsername :: Maybe Text -- ^ 
  , createEmailTaskVMServer :: Maybe Text -- ^ 
  , createEmailTaskVMPort :: Maybe Int -- ^ 
  , createEmailTaskVMEnableSsl :: Maybe Bool -- ^ 
  , createEmailTaskVMName :: Maybe Text -- ^ 
  , createEmailTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createEmailTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateEmailTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createEmailTaskVM")
instance ToJSON CreateEmailTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createEmailTaskVM")
instance ToSchema CreateEmailTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createEmailTaskVM"


-- | 
data CreateEndpointVM = CreateEndpointVM
  { createEndpointVMBearerToken :: Maybe Text -- ^ 
  , createEndpointVMUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateEndpointVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createEndpointVM")
instance ToJSON CreateEndpointVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createEndpointVM")
instance ToSchema CreateEndpointVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createEndpointVM"


-- | 
data CreateExportReportTaskVM = CreateExportReportTaskVM
  { createExportReportTaskVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  , createExportReportTaskVMFormat :: Maybe ExportFormat -- ^ 
  , createExportReportTaskVMPagesCount :: Maybe Int -- ^ 
  , createExportReportTaskVMName :: Maybe Text -- ^ 
  , createExportReportTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createExportReportTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateExportReportTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createExportReportTaskVM")
instance ToJSON CreateExportReportTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createExportReportTaskVM")
instance ToSchema CreateExportReportTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createExportReportTaskVM"


-- | 
data CreateExportTemplateTaskVM = CreateExportTemplateTaskVM
  { createExportTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , createExportTemplateTaskVMName :: Maybe Text -- ^ 
  , createExportTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createExportTemplateTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateExportTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createExportTemplateTaskVM")
instance ToJSON CreateExportTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createExportTemplateTaskVM")
instance ToSchema CreateExportTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createExportTemplateTaskVM"


-- | 
data CreateFetchTaskVM = CreateFetchTaskVM
  { createFetchTaskVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
  , createFetchTaskVMConnectionString :: Text -- ^ 
  , createFetchTaskVMName :: Maybe Text -- ^ 
  , createFetchTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createFetchTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateFetchTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createFetchTaskVM")
instance ToJSON CreateFetchTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createFetchTaskVM")
instance ToSchema CreateFetchTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createFetchTaskVM"


-- | 
data CreateGroupAdminVM = CreateGroupAdminVM
  { createGroupAdminVMOwnerId :: Maybe Text -- ^ 
  , createGroupAdminVMName :: Text -- ^ 
  , createGroupAdminVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupAdminVM")
instance ToJSON CreateGroupAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupAdminVM")
instance ToSchema CreateGroupAdminVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createGroupAdminVM"


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
data CreatePrepareTemplateTaskVM = CreatePrepareTemplateTaskVM
  { createPrepareTemplateTaskVMExports :: Maybe [CreateExportReportTaskVM] -- ^ 
  , createPrepareTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , createPrepareTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , createPrepareTemplateTaskVMName :: Maybe Text -- ^ 
  , createPrepareTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createPrepareTemplateTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreatePrepareTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createPrepareTemplateTaskVM")
instance ToJSON CreatePrepareTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createPrepareTemplateTaskVM")
instance ToSchema CreatePrepareTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createPrepareTemplateTaskVM"


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
data CreateTaskBaseVM = CreateTaskBaseVM
  { createTaskBaseVMName :: Maybe Text -- ^ 
  , createTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , createTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTaskBaseVM")
instance ToJSON CreateTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTaskBaseVM")
instance ToSchema CreateTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createTaskBaseVM"


-- | 
data CreateTransformTaskBaseVM = CreateTransformTaskBaseVM
  { createTransformTaskBaseVMLocale :: Maybe Text -- ^ 
  , createTransformTaskBaseVMInputFile :: Maybe InputFileVM -- ^ 
  , createTransformTaskBaseVMOutputFile :: Maybe OutputFileVM -- ^ 
  , createTransformTaskBaseVMTransports :: Maybe [CreateTransportTaskBaseVM] -- ^ 
  , createTransformTaskBaseVMName :: Maybe Text -- ^ 
  , createTransformTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , createTransformTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTransformTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTransformTaskBaseVM")
instance ToJSON CreateTransformTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTransformTaskBaseVM")
instance ToSchema CreateTransformTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createTransformTaskBaseVM"


-- | 
data CreateTransportTaskBaseVM = CreateTransportTaskBaseVM
  { createTransportTaskBaseVMFiles :: Maybe [InputFileVM] -- ^ 
  , createTransportTaskBaseVMName :: Maybe Text -- ^ 
  , createTransportTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , createTransportTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTransportTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTransportTaskBaseVM")
instance ToJSON CreateTransportTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTransportTaskBaseVM")
instance ToSchema CreateTransportTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createTransportTaskBaseVM"


-- | 
data CreateWebhookTaskVM = CreateWebhookTaskVM
  { createWebhookTaskVMEndpoints :: Maybe [CreateEndpointVM] -- ^ 
  , createWebhookTaskVMName :: Maybe Text -- ^ 
  , createWebhookTaskVMSubscriptionId :: Maybe Text -- ^ 
  , createWebhookTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateWebhookTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createWebhookTaskVM")
instance ToJSON CreateWebhookTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createWebhookTaskVM")
instance ToSchema CreateWebhookTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "createWebhookTaskVM"


-- | 
data DataSourceAdministrate = DataSourceAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceAdministrate")
instance ToJSON DataSourceAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceAdministrate")
instance ToSchema DataSourceAdministrate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceAdministrate"


-- | 
data DataSourceConnectionType = DataSourceConnectionType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceConnectionType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceConnectionType")
instance ToJSON DataSourceConnectionType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceConnectionType")
instance ToSchema DataSourceConnectionType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceConnectionType"


-- | 
data DataSourceCreate = DataSourceCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceCreate")
instance ToJSON DataSourceCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceCreate")
instance ToSchema DataSourceCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceCreate"


-- | 
data DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission = DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission
  { dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionCreate :: Maybe DataSourceCreate -- ^ 
  , dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionDelete :: Maybe DataSourceDelete -- ^ 
  , dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionExecute :: Maybe DataSourceExecute -- ^ 
  , dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionGet :: Maybe DataSourceGet -- ^ 
  , dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionUpdate :: Maybe DataSourceUpdate -- ^ 
  , dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionAdministrate :: Maybe DataSourceAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission")
instance ToJSON DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission")
instance ToSchema DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission"


-- | 
data DataSourceDelete = DataSourceDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceDelete")
instance ToJSON DataSourceDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceDelete")
instance ToSchema DataSourceDelete where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceDelete"


-- | 
data DataSourceExecute = DataSourceExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceExecute")
instance ToJSON DataSourceExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceExecute")
instance ToSchema DataSourceExecute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceExecute"


-- | 
data DataSourceGet = DataSourceGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceGet")
instance ToJSON DataSourceGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceGet")
instance ToSchema DataSourceGet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceGet"


-- | 
data DataSourcePermission = DataSourcePermission
  { dataSourcePermissionCreate :: Maybe DataSourceCreate -- ^ 
  , dataSourcePermissionDelete :: Maybe DataSourceDelete -- ^ 
  , dataSourcePermissionExecute :: Maybe DataSourceExecute -- ^ 
  , dataSourcePermissionGet :: Maybe DataSourceGet -- ^ 
  , dataSourcePermissionUpdate :: Maybe DataSourceUpdate -- ^ 
  , dataSourcePermissionAdministrate :: Maybe DataSourceAdministrate -- ^ 
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
data DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions = DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions
  { dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionsOwnerId :: Maybe Text -- ^ 
  , dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionsOwner :: Maybe DataSourcePermission -- ^ 
  , dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionsGroups :: Maybe (Map.Map String DataSourcePermission) -- ^ 
  , dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionsOther :: Maybe DataSourcePermission -- ^ 
  , dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissionsAnon :: Maybe DataSourcePermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions")
instance ToJSON DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions")
instance ToSchema DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions"


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
data DataSourceSorting = DataSourceSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceSorting where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceSorting")
instance ToJSON DataSourceSorting where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceSorting")
instance ToSchema DataSourceSorting where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceSorting"


-- | 
data DataSourceStatus = DataSourceStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceStatus")
instance ToJSON DataSourceStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceStatus")
instance ToSchema DataSourceStatus where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceStatus"


-- | 
data DataSourceUpdate = DataSourceUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceUpdate")
instance ToJSON DataSourceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceUpdate")
instance ToSchema DataSourceUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "dataSourceUpdate"


-- | 
data DataSourceVM = DataSourceVM
  { dataSourceVMId :: Maybe Text -- ^ 
  , dataSourceVMName :: Maybe Text -- ^ 
  , dataSourceVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
  , dataSourceVMConnectionString :: Maybe Text -- ^ 
  , dataSourceVMDataStructure :: Maybe Text -- ^ 
  , dataSourceVMSubscriptionId :: Maybe Text -- ^ 
  , dataSourceVMEditedTime :: Maybe UTCTime -- ^ 
  , dataSourceVMEditorUserId :: Maybe Text -- ^ 
  , dataSourceVMCreatedTime :: Maybe UTCTime -- ^ 
  , dataSourceVMCreatorUserId :: Maybe Text -- ^ 
  , dataSourceVMStatus :: Maybe DataSourceStatus -- ^ 
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
data EmailTaskVM = EmailTaskVM
  { emailTaskVMBody :: Maybe Text -- ^ 
  , emailTaskVMIsBodyHtml :: Maybe Bool -- ^ 
  , emailTaskVMSubject :: Maybe Text -- ^ 
  , emailTaskVMTo :: Maybe [Text] -- ^ 
  , emailTaskVMFrom :: Maybe Text -- ^ 
  , emailTaskVMUsername :: Maybe Text -- ^ 
  , emailTaskVMServer :: Maybe Text -- ^ 
  , emailTaskVMPort :: Maybe Int -- ^ 
  , emailTaskVMEnableSsl :: Maybe Bool -- ^ 
  , emailTaskVMName :: Maybe Text -- ^ 
  , emailTaskVMSubscriptionId :: Maybe Text -- ^ 
  , emailTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EmailTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "emailTaskVM")
instance ToJSON EmailTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "emailTaskVM")
instance ToSchema EmailTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "emailTaskVM"


-- | 
data EndpointVM = EndpointVM
  { endpointVMUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EndpointVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "endpointVM")
instance ToJSON EndpointVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "endpointVM")
instance ToSchema EndpointVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "endpointVM"


-- | 
data EntityVM = EntityVM
  { entityVMId :: Maybe Text -- ^ 
  , entityVMCreatedTime :: Maybe UTCTime -- ^ 
  , entityVMCreatorUserId :: Maybe Text -- ^ 
  , entityVMEditedTime :: Maybe UTCTime -- ^ 
  , entityVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityVM")
instance ToJSON EntityVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityVM")
instance ToSchema EntityVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "entityVM"


-- | 
data ExportCreateAdminVM = ExportCreateAdminVM
  { exportCreateAdminVMOwnerId :: Text -- ^ 
  , exportCreateAdminVMParentId :: Text -- ^ 
  , exportCreateAdminVMTemplateId :: Maybe Text -- ^ 
  , exportCreateAdminVMName :: Maybe Text -- ^ 
  , exportCreateAdminVMTags :: Maybe [Text] -- ^ 
  , exportCreateAdminVMIcon :: Maybe Text -- ^ 
  , exportCreateAdminVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportCreateAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportCreateAdminVM")
instance ToJSON ExportCreateAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportCreateAdminVM")
instance ToSchema ExportCreateAdminVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportCreateAdminVM"


-- | 
data ExportCreateVM = ExportCreateVM
  { exportCreateVMTemplateId :: Maybe Text -- ^ 
  , exportCreateVMName :: Maybe Text -- ^ 
  , exportCreateVMTags :: Maybe [Text] -- ^ 
  , exportCreateVMIcon :: Maybe Text -- ^ 
  , exportCreateVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportCreateVM")
instance ToJSON ExportCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportCreateVM")
instance ToSchema ExportCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportCreateVM"


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
data ExportFormat = ExportFormat
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportFormat where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportFormat")
instance ToJSON ExportFormat where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportFormat")
instance ToSchema ExportFormat where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportFormat"


-- | 
data ExportReportTaskVM = ExportReportTaskVM
  { exportReportTaskVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  , exportReportTaskVMFormat :: Maybe ExportFormat -- ^ 
  , exportReportTaskVMPagesCount :: Maybe Int -- ^ 
  , exportReportTaskVMName :: Maybe Text -- ^ 
  , exportReportTaskVMSubscriptionId :: Maybe Text -- ^ 
  , exportReportTaskVMType :: Maybe TaskType -- ^ 
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
data ExportReportVM = ExportReportVM
  { exportReportVMFileName :: Maybe Text -- ^ 
  , exportReportVMFolderId :: Maybe Text -- ^ 
  , exportReportVMLocale :: Maybe Text -- ^ 
  , exportReportVMPagesCount :: Maybe Int -- ^ 
  , exportReportVMFormat :: Maybe ExportFormat -- ^ 
  , exportReportVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportReportVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportReportVM")
instance ToJSON ExportReportVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportReportVM")
instance ToSchema ExportReportVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportReportVM"


-- | 
data ExportTemplateTaskVM = ExportTemplateTaskVM
  { exportTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , exportTemplateTaskVMName :: Maybe Text -- ^ 
  , exportTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , exportTemplateTaskVMType :: Maybe TaskType -- ^ 
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
data ExportTemplateVM = ExportTemplateVM
  { exportTemplateVMFileName :: Maybe Text -- ^ 
  , exportTemplateVMFolderId :: Maybe Text -- ^ 
  , exportTemplateVMLocale :: Maybe Text -- ^ 
  , exportTemplateVMPagesCount :: Maybe Int -- ^ 
  , exportTemplateVMFormat :: Maybe ExportFormat -- ^ 
  , exportTemplateVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  , exportTemplateVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportTemplateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportTemplateVM")
instance ToJSON ExportTemplateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportTemplateVM")
instance ToSchema ExportTemplateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportTemplateVM"


-- | 
data ExportVM = ExportVM
  { exportVMFormat :: Maybe ExportFormat -- ^ 
  , exportVMReportId :: Maybe Text -- ^ 
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
data ExportVMFilesVMBase = ExportVMFilesVMBase
  { exportVMFilesVMBaseFiles :: Maybe [ExportVM] -- ^ 
  , exportVMFilesVMBaseCount :: Maybe Integer -- ^ 
  , exportVMFilesVMBaseSkip :: Maybe Int -- ^ 
  , exportVMFilesVMBaseTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportVMFilesVMBase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportVMFilesVMBase")
instance ToJSON ExportVMFilesVMBase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportVMFilesVMBase")
instance ToSchema ExportVMFilesVMBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "exportVMFilesVMBase"


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
data FetchTaskVM = FetchTaskVM
  { fetchTaskVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
  , fetchTaskVMConnectionString :: Text -- ^ 
  , fetchTaskVMName :: Maybe Text -- ^ 
  , fetchTaskVMSubscriptionId :: Maybe Text -- ^ 
  , fetchTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FetchTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fetchTaskVM")
instance ToJSON FetchTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fetchTaskVM")
instance ToSchema FetchTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fetchTaskVM"


-- | 
data FileAdministrate = FileAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileAdministrate")
instance ToJSON FileAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileAdministrate")
instance ToSchema FileAdministrate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileAdministrate"


-- | 
data FileCreate = FileCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileCreate")
instance ToJSON FileCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileCreate")
instance ToSchema FileCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileCreate"


-- | 
data FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission = FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission
  { fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionCreate :: Maybe FileCreate -- ^ 
  , fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionDelete :: Maybe FileDelete -- ^ 
  , fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionExecute :: Maybe FileExecute -- ^ 
  , fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionGet :: Maybe FileGet -- ^ 
  , fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionUpdate :: Maybe FileUpdate -- ^ 
  , fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionAdministrate :: Maybe FileAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission")
instance ToJSON FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission")
instance ToSchema FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission"


-- | 
data FileCreateVM = FileCreateVM
  { fileCreateVMName :: Maybe Text -- ^ 
  , fileCreateVMTags :: Maybe [Text] -- ^ 
  , fileCreateVMIcon :: Maybe Text -- ^ 
  , fileCreateVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileCreateVM")
instance ToJSON FileCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileCreateVM")
instance ToSchema FileCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileCreateVM"


-- | 
data FileDelete = FileDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileDelete")
instance ToJSON FileDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileDelete")
instance ToSchema FileDelete where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileDelete"


-- | 
data FileExecute = FileExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileExecute")
instance ToJSON FileExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileExecute")
instance ToSchema FileExecute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileExecute"


-- | 
data FileGet = FileGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileGet")
instance ToJSON FileGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileGet")
instance ToSchema FileGet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileGet"


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
data FileKind = FileKind
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileKind where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileKind")
instance ToJSON FileKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileKind")
instance ToSchema FileKind where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileKind"


-- | 
data FilePermission = FilePermission
  { filePermissionCreate :: Maybe FileCreate -- ^ 
  , filePermissionDelete :: Maybe FileDelete -- ^ 
  , filePermissionExecute :: Maybe FileExecute -- ^ 
  , filePermissionGet :: Maybe FileGet -- ^ 
  , filePermissionUpdate :: Maybe FileUpdate -- ^ 
  , filePermissionAdministrate :: Maybe FileAdministrate -- ^ 
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
data FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions = FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions
  { filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionsOwnerId :: Maybe Text -- ^ 
  , filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionsOwner :: Maybe FilePermission -- ^ 
  , filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionsGroups :: Maybe (Map.Map String FilePermission) -- ^ 
  , filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionsOther :: Maybe FilePermission -- ^ 
  , filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissionsAnon :: Maybe FilePermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions")
instance ToJSON FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions")
instance ToSchema FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "filePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions"


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
data FileSorting = FileSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileSorting where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileSorting")
instance ToJSON FileSorting where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileSorting")
instance ToSchema FileSorting where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileSorting"


-- | 
data FileStatus = FileStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileStatus")
instance ToJSON FileStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileStatus")
instance ToSchema FileStatus where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileStatus"


-- | 
data FileStatusReason = FileStatusReason
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatusReason where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileStatusReason")
instance ToJSON FileStatusReason where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileStatusReason")
instance ToSchema FileStatusReason where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileStatusReason"


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
data FileType = FileType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileType")
instance ToJSON FileType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileType")
instance ToSchema FileType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileType"


-- | 
data FileUpdate = FileUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileUpdate")
instance ToJSON FileUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileUpdate")
instance ToSchema FileUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileUpdate"


-- | 
data FileVM = FileVM
  { fileVMName :: Maybe Text -- ^ 
  , fileVMParentId :: Maybe Text -- ^ 
  , fileVMTags :: Maybe [Text] -- ^ 
  , fileVMIcon :: Maybe Text -- ^ 
  , fileVMType :: Maybe FileType -- ^ 
  , fileVMSize :: Maybe Integer -- ^ 
  , fileVMSubscriptionId :: Maybe Text -- ^ 
  , fileVMStatus :: Maybe FileStatus -- ^ 
  , fileVMStatusReason :: Maybe FileStatusReason -- ^ 
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
data FileVMFilesVMBase = FileVMFilesVMBase
  { fileVMFilesVMBaseFiles :: Maybe [FileVM] -- ^ 
  , fileVMFilesVMBaseCount :: Maybe Integer -- ^ 
  , fileVMFilesVMBaseSkip :: Maybe Int -- ^ 
  , fileVMFilesVMBaseTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileVMFilesVMBase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileVMFilesVMBase")
instance ToJSON FileVMFilesVMBase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileVMFilesVMBase")
instance ToSchema FileVMFilesVMBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "fileVMFilesVMBase"


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
data FolderCreateVM = FolderCreateVM
  { folderCreateVMName :: Maybe Text -- ^ 
  , folderCreateVMTags :: Maybe [Text] -- ^ 
  , folderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderCreateVM")
instance ToJSON FolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderCreateVM")
instance ToSchema FolderCreateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "folderCreateVM"


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
data GroupAdministrate = GroupAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupAdministrate")
instance ToJSON GroupAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupAdministrate")
instance ToSchema GroupAdministrate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupAdministrate"


-- | 
data GroupCreate = GroupCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupCreate")
instance ToJSON GroupCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupCreate")
instance ToSchema GroupCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupCreate"


-- | 
data GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission = GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission
  { groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionCreate :: Maybe GroupCreate -- ^ 
  , groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionDelete :: Maybe GroupDelete -- ^ 
  , groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionExecute :: Maybe GroupExecute -- ^ 
  , groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionGet :: Maybe GroupGet -- ^ 
  , groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionUpdate :: Maybe GroupUpdate -- ^ 
  , groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionAdministrate :: Maybe GroupAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission")
instance ToJSON GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission")
instance ToSchema GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission"


-- | 
data GroupDelete = GroupDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupDelete")
instance ToJSON GroupDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupDelete")
instance ToSchema GroupDelete where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupDelete"


-- | 
data GroupExecute = GroupExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupExecute")
instance ToJSON GroupExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupExecute")
instance ToSchema GroupExecute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupExecute"


-- | 
data GroupGet = GroupGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupGet")
instance ToJSON GroupGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupGet")
instance ToSchema GroupGet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupGet"


-- | 
data GroupPermission = GroupPermission
  { groupPermissionCreate :: Maybe GroupCreate -- ^ 
  , groupPermissionDelete :: Maybe GroupDelete -- ^ 
  , groupPermissionExecute :: Maybe GroupExecute -- ^ 
  , groupPermissionGet :: Maybe GroupGet -- ^ 
  , groupPermissionUpdate :: Maybe GroupUpdate -- ^ 
  , groupPermissionAdministrate :: Maybe GroupAdministrate -- ^ 
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
data GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions = GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions
  { groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionsOwnerId :: Maybe Text -- ^ 
  , groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionsOwner :: Maybe GroupPermission -- ^ 
  , groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionsGroups :: Maybe (Map.Map String GroupPermission) -- ^ 
  , groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionsOther :: Maybe GroupPermission -- ^ 
  , groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissionsAnon :: Maybe GroupPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions")
instance ToJSON GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions")
instance ToSchema GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions"


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
data GroupUpdate = GroupUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupUpdate")
instance ToJSON GroupUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupUpdate")
instance ToSchema GroupUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "groupUpdate"


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
data InputFileVM = InputFileVM
  { inputFileVMEntityId :: Maybe Text -- ^ 
  , inputFileVMFileName :: Maybe Text -- ^ 
  , inputFileVMType :: Maybe FileKind -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InputFileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inputFileVM")
instance ToJSON InputFileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inputFileVM")
instance ToSchema InputFileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "inputFileVM"


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
data MyPermissionsVM = MyPermissionsVM
  { myPermissionsVMSubscription :: Maybe SubscriptionPermission -- ^ 
  , myPermissionsVMFiles :: Maybe FilePermission -- ^ 
  , myPermissionsVMDatasources :: Maybe DataSourcePermission -- ^ 
  , myPermissionsVMGroups :: Maybe GroupPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MyPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "myPermissionsVM")
instance ToJSON MyPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "myPermissionsVM")
instance ToSchema MyPermissionsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "myPermissionsVM"


-- | 
data OutputFileVM = OutputFileVM
  { outputFileVMFileName :: Maybe Text -- ^ 
  , outputFileVMFolderId :: Maybe Text -- ^ 
  , outputFileVMType :: Maybe FileKind -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OutputFileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "outputFileVM")
instance ToJSON OutputFileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "outputFileVM")
instance ToSchema OutputFileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "outputFileVM"


-- | 
data PrepareTemplateTaskVM = PrepareTemplateTaskVM
  { prepareTemplateTaskVMExports :: Maybe [ExportReportTaskVM] -- ^ 
  , prepareTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , prepareTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , prepareTemplateTaskVMName :: Maybe Text -- ^ 
  , prepareTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , prepareTemplateTaskVMType :: Maybe TaskType -- ^ 
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
data PrepareTemplateVM = PrepareTemplateVM
  { prepareTemplateVMName :: Maybe Text -- ^ 
  , prepareTemplateVMLocale :: Maybe Text -- ^ 
  , prepareTemplateVMParentFolderId :: Maybe Text -- ^ 
  , prepareTemplateVMPagesCount :: Maybe Int -- ^ 
  , prepareTemplateVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PrepareTemplateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "prepareTemplateVM")
instance ToJSON PrepareTemplateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "prepareTemplateVM")
instance ToSchema PrepareTemplateVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "prepareTemplateVM"


-- | 
newtype ProblemDetails = ProblemDetails { unProblemDetails :: (Map.Map Text AnyType) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data ProfileVisibility = ProfileVisibility
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProfileVisibility where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "profileVisibility")
instance ToJSON ProfileVisibility where
  toJSON = genericToJSON (removeFieldLabelPrefix False "profileVisibility")
instance ToSchema ProfileVisibility where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "profileVisibility"


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
data ReportCreateAdminVM = ReportCreateAdminVM
  { reportCreateAdminVMOwnerId :: Text -- ^ 
  , reportCreateAdminVMParentId :: Text -- ^ 
  , reportCreateAdminVMName :: Maybe Text -- ^ 
  , reportCreateAdminVMTags :: Maybe [Text] -- ^ 
  , reportCreateAdminVMIcon :: Maybe Text -- ^ 
  , reportCreateAdminVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportCreateAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportCreateAdminVM")
instance ToJSON ReportCreateAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportCreateAdminVM")
instance ToSchema ReportCreateAdminVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportCreateAdminVM"


-- | 
data ReportCreateVM = ReportCreateVM
  { reportCreateVMTemplateId :: Maybe Text -- ^ 
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
  , reportInfoSaveMode :: Maybe SaveMode -- ^ 
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
data ReportVMFilesVMBase = ReportVMFilesVMBase
  { reportVMFilesVMBaseFiles :: Maybe [ReportVM] -- ^ 
  , reportVMFilesVMBaseCount :: Maybe Integer -- ^ 
  , reportVMFilesVMBaseSkip :: Maybe Int -- ^ 
  , reportVMFilesVMBaseTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportVMFilesVMBase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportVMFilesVMBase")
instance ToJSON ReportVMFilesVMBase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportVMFilesVMBase")
instance ToSchema ReportVMFilesVMBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "reportVMFilesVMBase"


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
data RunEmailTaskVM = RunEmailTaskVM
  { runEmailTaskVMPassword :: Maybe Text -- ^ 
  , runEmailTaskVMBody :: Maybe Text -- ^ 
  , runEmailTaskVMIsBodyHtml :: Maybe Bool -- ^ 
  , runEmailTaskVMSubject :: Maybe Text -- ^ 
  , runEmailTaskVMTo :: Maybe [Text] -- ^ 
  , runEmailTaskVMFrom :: Maybe Text -- ^ 
  , runEmailTaskVMUsername :: Maybe Text -- ^ 
  , runEmailTaskVMServer :: Maybe Text -- ^ 
  , runEmailTaskVMPort :: Maybe Int -- ^ 
  , runEmailTaskVMEnableSsl :: Maybe Bool -- ^ 
  , runEmailTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runEmailTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunEmailTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runEmailTaskVM")
instance ToJSON RunEmailTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runEmailTaskVM")
instance ToSchema RunEmailTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runEmailTaskVM"


-- | 
data RunEndpointVM = RunEndpointVM
  { runEndpointVMUrl :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunEndpointVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runEndpointVM")
instance ToJSON RunEndpointVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runEndpointVM")
instance ToSchema RunEndpointVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runEndpointVM"


-- | 
data RunExportReportTaskVM = RunExportReportTaskVM
  { runExportReportTaskVMExportParameters :: Maybe (Map.Map String Text) -- ^ 
  , runExportReportTaskVMFormat :: Maybe ExportFormat -- ^ 
  , runExportReportTaskVMPagesCount :: Maybe Int -- ^ 
  , runExportReportTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runExportReportTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunExportReportTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runExportReportTaskVM")
instance ToJSON RunExportReportTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runExportReportTaskVM")
instance ToSchema RunExportReportTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runExportReportTaskVM"


-- | 
data RunExportTemplateTaskVM = RunExportTemplateTaskVM
  { runExportTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , runExportTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runExportTemplateTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunExportTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runExportTemplateTaskVM")
instance ToJSON RunExportTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runExportTemplateTaskVM")
instance ToSchema RunExportTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runExportTemplateTaskVM"


-- | 
data RunFetchTaskVM = RunFetchTaskVM
  { runFetchTaskVMConnectionType :: Maybe DataSourceConnectionType -- ^ 
  , runFetchTaskVMConnectionString :: Text -- ^ 
  , runFetchTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runFetchTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunFetchTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runFetchTaskVM")
instance ToJSON RunFetchTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runFetchTaskVM")
instance ToSchema RunFetchTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runFetchTaskVM"


-- | 
data RunInputFileVM = RunInputFileVM
  { runInputFileVMContent :: Maybe Text -- ^ 
  , runInputFileVMEntityId :: Maybe Text -- ^ 
  , runInputFileVMFileName :: Maybe Text -- ^ 
  , runInputFileVMType :: Maybe FileKind -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunInputFileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runInputFileVM")
instance ToJSON RunInputFileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runInputFileVM")
instance ToSchema RunInputFileVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runInputFileVM"


-- | 
data RunPrepareTemplateTaskVM = RunPrepareTemplateTaskVM
  { runPrepareTemplateTaskVMExports :: Maybe [RunExportReportTaskVM] -- ^ 
  , runPrepareTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , runPrepareTemplateTaskVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , runPrepareTemplateTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runPrepareTemplateTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunPrepareTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runPrepareTemplateTaskVM")
instance ToJSON RunPrepareTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runPrepareTemplateTaskVM")
instance ToSchema RunPrepareTemplateTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runPrepareTemplateTaskVM"


-- | 
data RunTaskBaseVM = RunTaskBaseVM
  { runTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , runTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runTaskBaseVM")
instance ToJSON RunTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runTaskBaseVM")
instance ToSchema RunTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runTaskBaseVM"


-- | 
data RunTransformTaskBaseVM = RunTransformTaskBaseVM
  { runTransformTaskBaseVMLocale :: Maybe Text -- ^ 
  , runTransformTaskBaseVMInputFile :: Maybe RunInputFileVM -- ^ 
  , runTransformTaskBaseVMOutputFile :: Maybe OutputFileVM -- ^ 
  , runTransformTaskBaseVMTransports :: Maybe [RunTransportTaskBaseVM] -- ^ 
  , runTransformTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , runTransformTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunTransformTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runTransformTaskBaseVM")
instance ToJSON RunTransformTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runTransformTaskBaseVM")
instance ToSchema RunTransformTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runTransformTaskBaseVM"


-- | 
data RunTransportTaskBaseVM = RunTransportTaskBaseVM
  { runTransportTaskBaseVMFiles :: Maybe [RunInputFileVM] -- ^ 
  , runTransportTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , runTransportTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunTransportTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runTransportTaskBaseVM")
instance ToJSON RunTransportTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runTransportTaskBaseVM")
instance ToSchema RunTransportTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runTransportTaskBaseVM"


-- | 
data RunWebhookTaskVM = RunWebhookTaskVM
  { runWebhookTaskVMEndpoints :: Maybe [RunEndpointVM] -- ^ 
  , runWebhookTaskVMSubscriptionId :: Maybe Text -- ^ 
  , runWebhookTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunWebhookTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runWebhookTaskVM")
instance ToJSON RunWebhookTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runWebhookTaskVM")
instance ToSchema RunWebhookTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "runWebhookTaskVM"


-- | 
data SaveMode = SaveMode
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SaveMode where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "saveMode")
instance ToJSON SaveMode where
  toJSON = genericToJSON (removeFieldLabelPrefix False "saveMode")
instance ToSchema SaveMode where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "saveMode"


-- | 
data ServerConfigurationVM = ServerConfigurationVM
  { serverConfigurationVMTitle :: Maybe Text -- ^ 
  , serverConfigurationVMCorporateServerMode :: Maybe Bool -- ^ 
  , serverConfigurationVMAppMixins :: Maybe AppMixins -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ServerConfigurationVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serverConfigurationVM")
instance ToJSON ServerConfigurationVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serverConfigurationVM")
instance ToSchema ServerConfigurationVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "serverConfigurationVM"


-- | 
data SubscriptionAdministrate = SubscriptionAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionAdministrate")
instance ToJSON SubscriptionAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionAdministrate")
instance ToSchema SubscriptionAdministrate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionAdministrate"


-- | 
data SubscriptionCreate = SubscriptionCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionCreate")
instance ToJSON SubscriptionCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionCreate")
instance ToSchema SubscriptionCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionCreate"


-- | 
data SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission = SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission
  { subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionCreate :: Maybe SubscriptionCreate -- ^ 
  , subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionDelete :: Maybe SubscriptionDelete -- ^ 
  , subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionExecute :: Maybe SubscriptionExecute -- ^ 
  , subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionGet :: Maybe SubscriptionGet -- ^ 
  , subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionUpdate :: Maybe SubscriptionUpdate -- ^ 
  , subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionAdministrate :: Maybe SubscriptionAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission")
instance ToJSON SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission")
instance ToSchema SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission"


-- | 
data SubscriptionDelete = SubscriptionDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionDelete")
instance ToJSON SubscriptionDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionDelete")
instance ToSchema SubscriptionDelete where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionDelete"


-- | 
data SubscriptionExecute = SubscriptionExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionExecute")
instance ToJSON SubscriptionExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionExecute")
instance ToSchema SubscriptionExecute where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionExecute"


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
data SubscriptionGet = SubscriptionGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionGet")
instance ToJSON SubscriptionGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionGet")
instance ToSchema SubscriptionGet where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionGet"


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
  { subscriptionPermissionCreate :: Maybe SubscriptionCreate -- ^ 
  , subscriptionPermissionDelete :: Maybe SubscriptionDelete -- ^ 
  , subscriptionPermissionExecute :: Maybe SubscriptionExecute -- ^ 
  , subscriptionPermissionGet :: Maybe SubscriptionGet -- ^ 
  , subscriptionPermissionUpdate :: Maybe SubscriptionUpdate -- ^ 
  , subscriptionPermissionAdministrate :: Maybe SubscriptionAdministrate -- ^ 
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
data SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions = SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions
  { subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionsOwnerId :: Maybe Text -- ^ 
  , subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionsOwner :: Maybe SubscriptionPermission -- ^ 
  , subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionsGroups :: Maybe (Map.Map String SubscriptionPermission) -- ^ 
  , subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionsOther :: Maybe SubscriptionPermission -- ^ 
  , subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissionsAnon :: Maybe SubscriptionPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions")
instance ToJSON SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions")
instance ToSchema SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions"


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
  , subscriptionPlanVMTimePeriodType :: Maybe TimePeriodType -- ^ 
  , subscriptionPlanVMTimePeriod :: Maybe Int -- ^ 
  , subscriptionPlanVMReadonlyTimeLimitType :: Maybe TimePeriodType -- ^ 
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
  , subscriptionPlanVMTasks :: Maybe TaskSettingsVM -- ^ 
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
data SubscriptionUpdate = SubscriptionUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionUpdate")
instance ToJSON SubscriptionUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionUpdate")
instance ToSchema SubscriptionUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "subscriptionUpdate"


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
data TaskBaseVM = TaskBaseVM
  { taskBaseVMId :: Maybe Text -- ^ 
  , taskBaseVMName :: Maybe Text -- ^ 
  , taskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , taskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskBaseVM")
instance ToJSON TaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskBaseVM")
instance ToSchema TaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "taskBaseVM"


-- | 
data TaskSettingsVM = TaskSettingsVM
  { taskSettingsVMPrepare :: Maybe Bool -- ^ 
  , taskSettingsVMExportTemplate :: Maybe Bool -- ^ 
  , taskSettingsVMExportReport :: Maybe Bool -- ^ 
  , taskSettingsVMSendViaEmail :: Maybe Bool -- ^ 
  , taskSettingsVMSendViaWebhook :: Maybe Bool -- ^ 
  , taskSettingsVMFetchData :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskSettingsVM")
instance ToJSON TaskSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskSettingsVM")
instance ToSchema TaskSettingsVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "taskSettingsVM"


-- | 
data TaskType = TaskType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskType")
instance ToJSON TaskType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskType")
instance ToSchema TaskType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "taskType"


-- | 
data TasksVM = TasksVM
  { tasksVMCount :: Maybe Integer -- ^ 
  , tasksVMSkip :: Maybe Int -- ^ 
  , tasksVMTake :: Maybe Int -- ^ 
  , tasksVMTasks :: Maybe [TaskBaseVM] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TasksVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tasksVM")
instance ToJSON TasksVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tasksVM")
instance ToSchema TasksVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "tasksVM"


-- | 
data TemplateCreateAdminVM = TemplateCreateAdminVM
  { templateCreateAdminVMOwnerId :: Text -- ^ 
  , templateCreateAdminVMParentId :: Text -- ^ 
  , templateCreateAdminVMName :: Maybe Text -- ^ 
  , templateCreateAdminVMTags :: Maybe [Text] -- ^ 
  , templateCreateAdminVMIcon :: Maybe Text -- ^ 
  , templateCreateAdminVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateCreateAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateCreateAdminVM")
instance ToJSON TemplateCreateAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateCreateAdminVM")
instance ToSchema TemplateCreateAdminVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templateCreateAdminVM"


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
data TemplateVMFilesVMBase = TemplateVMFilesVMBase
  { templateVMFilesVMBaseFiles :: Maybe [TemplateVM] -- ^ 
  , templateVMFilesVMBaseCount :: Maybe Integer -- ^ 
  , templateVMFilesVMBaseSkip :: Maybe Int -- ^ 
  , templateVMFilesVMBaseTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateVMFilesVMBase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateVMFilesVMBase")
instance ToJSON TemplateVMFilesVMBase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateVMFilesVMBase")
instance ToSchema TemplateVMFilesVMBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "templateVMFilesVMBase"


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
data TimePeriodType = TimePeriodType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TimePeriodType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "timePeriodType")
instance ToJSON TimePeriodType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "timePeriodType")
instance ToSchema TimePeriodType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "timePeriodType"


-- | 
data TransformTaskBaseVM = TransformTaskBaseVM
  { transformTaskBaseVMLocale :: Maybe Text -- ^ 
  , transformTaskBaseVMInputFile :: Maybe InputFileVM -- ^ 
  , transformTaskBaseVMOutputFile :: Maybe OutputFileVM -- ^ 
  , transformTaskBaseVMTransports :: Maybe [TransportTaskBaseVM] -- ^ 
  , transformTaskBaseVMName :: Maybe Text -- ^ 
  , transformTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , transformTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransformTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "transformTaskBaseVM")
instance ToJSON TransformTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "transformTaskBaseVM")
instance ToSchema TransformTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "transformTaskBaseVM"


-- | 
data TransportTaskBaseVM = TransportTaskBaseVM
  { transportTaskBaseVMFiles :: Maybe [InputFileVM] -- ^ 
  , transportTaskBaseVMName :: Maybe Text -- ^ 
  , transportTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , transportTaskBaseVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TransportTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "transportTaskBaseVM")
instance ToJSON TransportTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "transportTaskBaseVM")
instance ToSchema TransportTaskBaseVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "transportTaskBaseVM"


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
  , updateDataSourcePermissionsVMAdministrate :: DataSourceAdministrate -- ^ 
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
  , updateFilePermissionsVMAdministrate :: FileAdministrate -- ^ 
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
  , updateGroupPermissionsVMAdministrate :: GroupAdministrate -- ^ 
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
  , updateSubscriptionPermissionsVMAdministrate :: SubscriptionAdministrate -- ^ 
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
  { updateUserSettingsVMProfileVisibility :: Maybe ProfileVisibility -- ^ 
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
  { userSettingsVMProfileVisibility :: Maybe ProfileVisibility -- ^ 
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


-- | 
data ValidationProblemDetails = ValidationProblemDetails
  { validationProblemDetailsErrors :: Maybe (Map.Map String [Text]) -- ^ 
  , validationProblemDetailsType :: Maybe Text -- ^ 
  , validationProblemDetailsTitle :: Maybe Text -- ^ 
  , validationProblemDetailsStatus :: Maybe Int -- ^ 
  , validationProblemDetailsDetail :: Maybe Text -- ^ 
  , validationProblemDetailsInstance :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ValidationProblemDetails where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "validationProblemDetails")
instance ToJSON ValidationProblemDetails where
  toJSON = genericToJSON (removeFieldLabelPrefix False "validationProblemDetails")
instance ToSchema ValidationProblemDetails where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "validationProblemDetails"


-- | 
data WebhookTaskVM = WebhookTaskVM
  { webhookTaskVMEndpoints :: Maybe [EndpointVM] -- ^ 
  , webhookTaskVMName :: Maybe Text -- ^ 
  , webhookTaskVMSubscriptionId :: Maybe Text -- ^ 
  , webhookTaskVMType :: Maybe TaskType -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON WebhookTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webhookTaskVM")
instance ToJSON WebhookTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webhookTaskVM")
instance ToSchema WebhookTaskVM where
  declareNamedSchema = Swagger.genericDeclareNamedSchema
    $ Swagger.fromAesonOptions
    $ removeFieldLabelPrefix False "webhookTaskVM"


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
