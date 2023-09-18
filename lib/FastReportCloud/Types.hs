{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module FastReportCloud.Types (
  AcceptAgreementsVM (..),
  AdminExportFolderCreateVM (..),
  AdminFolderCreateVM (..),
  AdminReportFolderCreateVM (..),
  AdminSubscriptionVM (..),
  AdminTemplateFolderCreateVM (..),
  ApiKeyVM (..),
  ApiKeysVM (..),
  AppMixins (..),
  AuditActionVM (..),
  AuditActionsVM (..),
  AuditFilePropertyChangedVM (..),
  AuditSubscriptionActionVM (..),
  AuditTaskActionVM (..),
  AuditType (..),
  AuthConfigVM (..),
  BreadcrumbsModel (..),
  BreadcrumbsVM (..),
  ClearNotificationsVM (..),
  ContactGroupVM (..),
  ContactGroupsVM (..),
  ContactVM (..),
  ContactsVM (..),
  CountVM (..),
  CreateApiKeyVM (..),
  CreateContactGroupVM (..),
  CreateContactVM (..),
  CreateDataSourceAdminVM (..),
  CreateDataSourceVM (..),
  CreateEmailTaskVM (..),
  CreateExportReportTaskVM (..),
  CreateExportTemplateTaskVM (..),
  CreateFTPUploadTaskVM (..),
  CreateFetchTaskVM (..),
  CreateGroupAdminVM (..),
  CreateGroupVM (..),
  CreatePrepareTemplateTaskVM (..),
  CreateSubscriptionInviteVM (..),
  CreateTaskBaseVM (..),
  CreateThumbnailReportTaskVM (..),
  CreateThumbnailTemplateTaskVM (..),
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
  DefaultPermissionsVM (..),
  DeleteApiKeyVM (..),
  EmailTaskVM (..),
  EntityType (..),
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
  FTPUploadTaskVM (..),
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
  FolderSizeVM (..),
  FolderTagsUpdateVM (..),
  FrontendApp (..),
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
  HttpValidationProblemDetails (..),
  InputFileVM (..),
  InvitedUser (..),
  MyPermissionsVM (..),
  OutputFileVM (..),
  PrepareTemplateTaskVM (..),
  PrepareTemplateVM (..),
  PreviewReportVM (..),
  PreviewTemplateVM (..),
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
  RunExportReportTaskVM (..),
  RunExportTemplateTaskVM (..),
  RunFTPUploadTaskVM (..),
  RunFetchTaskVM (..),
  RunInputFileVM (..),
  RunPrepareTemplateTaskVM (..),
  RunTaskBaseVM (..),
  RunThumbnailReportTaskVM (..),
  RunThumbnailTemplateTaskVM (..),
  RunTransformTaskBaseVM (..),
  RunTransportTaskBaseVM (..),
  RunWebhookTaskVM (..),
  SaveMode (..),
  SelectedFilesForDeletingVM (..),
  SelectedFilesVM (..),
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
  TaskAdministrate (..),
  TaskBaseVM (..),
  TaskCreate (..),
  TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission (..),
  TaskDelete (..),
  TaskExecute (..),
  TaskGet (..),
  TaskPermission (..),
  TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions (..),
  TaskPermissions (..),
  TaskPermissionsVM (..),
  TaskSettingsVM (..),
  TaskUpdate (..),
  TasksVM (..),
  TemplateCreateAdminVM (..),
  TemplateCreateVM (..),
  TemplateFolderCreateVM (..),
  TemplateVM (..),
  TemplateVMFilesVMBase (..),
  TemplatesVM (..),
  ThumbnailReportTaskVM (..),
  ThumbnailTemplateTaskVM (..),
  TimePeriodType (..),
  TransformTaskBaseVM (..),
  TransportTaskBaseVM (..),
  UpdateContactGroupVM (..),
  UpdateContactVM (..),
  UpdateDataSourceConnectionStringVM (..),
  UpdateDataSourcePermissionsVM (..),
  UpdateDataSourceSubscriptionVM (..),
  UpdateDefaultPermissionsVM (..),
  UpdateEmailTaskVM (..),
  UpdateExportReportTaskVM (..),
  UpdateExportTemplateTaskVM (..),
  UpdateFTPUploadTaskVM (..),
  UpdateFetchTaskVM (..),
  UpdateFileContentVM (..),
  UpdateFilePermissionsVM (..),
  UpdateGroupPermissionsVM (..),
  UpdatePrepareTemplateTaskVM (..),
  UpdateSubscriptionLocaleVM (..),
  UpdateSubscriptionPermissionsVM (..),
  UpdateTaskBaseVM (..),
  UpdateTaskPermissionsVM (..),
  UpdateThumbnailReportTaskVM (..),
  UpdateThumbnailTemplateTaskVM (..),
  UpdateTransformTaskBaseVM (..),
  UpdateTransportTaskBaseVM (..),
  UpdateUserProfileVM (..),
  UpdateUserSettingsVM (..),
  UpdateWebhookTaskVM (..),
  UserProfileVM (..),
  UserSettingsVM (..),
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
data AcceptAgreementsVM = AcceptAgreementsVM
  { acceptAgreementsVMSlaAccepted :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AcceptAgreementsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "acceptAgreementsVM")
instance ToJSON AcceptAgreementsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "acceptAgreementsVM")


-- | 
newtype AdminExportFolderCreateVM = AdminExportFolderCreateVM { unAdminExportFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminFolderCreateVM = AdminFolderCreateVM { unAdminFolderCreateVM :: FolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminReportFolderCreateVM = AdminReportFolderCreateVM { unAdminReportFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminSubscriptionVM = AdminSubscriptionVM { unAdminSubscriptionVM :: SubscriptionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminTemplateFolderCreateVM = AdminTemplateFolderCreateVM { unAdminTemplateFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
data ApiKeysVM = ApiKeysVM
  { apiKeysVMApiKeys :: Maybe [ApiKeyVM] -- ^ 
  , apiKeysVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ApiKeysVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiKeysVM")
instance ToJSON ApiKeysVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiKeysVM")


-- | 
data AppMixins = AppMixins
  { appMixinsHead :: Maybe Text -- ^ 
  , appMixinsBody :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AppMixins where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appMixins")
instance ToJSON AppMixins where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appMixins")


-- | 
data AuditActionVM = AuditActionVM
  { auditActionVMUserId :: Maybe Text -- ^ 
  , auditActionVMEntityId :: Maybe Text -- ^ 
  , auditActionVMSubscriptionId :: Maybe Text -- ^ 
  , auditActionVMType :: Maybe AuditType -- ^ 
  , auditActionVMId :: Maybe Text -- ^ 
  , auditActionVMCreatedTime :: Maybe UTCTime -- ^ 
  , auditActionVMCreatorUserId :: Maybe Text -- ^ 
  , auditActionVMName :: Maybe Text -- ^ 
  , auditActionVMAdminAction :: Maybe Bool -- ^ 
  , auditActionVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AuditActionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditActionVM")
instance ToJSON AuditActionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditActionVM")


-- | 
data AuditActionsVM = AuditActionsVM
  { auditActionsVMItems :: Maybe [AuditActionVM] -- ^ 
  , auditActionsVMCount :: Maybe Integer -- ^ 
  , auditActionsVMSkip :: Maybe Int -- ^ 
  , auditActionsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AuditActionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditActionsVM")
instance ToJSON AuditActionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditActionsVM")


-- | 
newtype AuditFilePropertyChangedVM = AuditFilePropertyChangedVM { unAuditFilePropertyChangedVM :: AuditActionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditSubscriptionActionVM = AuditSubscriptionActionVM { unAuditSubscriptionActionVM :: AuditActionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditTaskActionVM = AuditTaskActionVM { unAuditTaskActionVM :: AuditActionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data AuditType = AuditType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AuditType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditType")
instance ToJSON AuditType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditType")


-- | 
data AuthConfigVM = AuthConfigVM
  { authConfigVMUseLocal :: Maybe Bool -- ^ 
  , authConfigVMUseOpenId :: Maybe Bool -- ^ 
  , authConfigVMAuthority :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AuthConfigVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "authConfigVM")
instance ToJSON AuthConfigVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "authConfigVM")


-- | 
data BreadcrumbsModel = BreadcrumbsModel
  { breadcrumbsModelId :: Maybe Text -- ^ 
  , breadcrumbsModelName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BreadcrumbsModel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "breadcrumbsModel")
instance ToJSON BreadcrumbsModel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "breadcrumbsModel")


-- | 
data BreadcrumbsVM = BreadcrumbsVM
  { breadcrumbsVMBreadcrumbs :: Maybe [BreadcrumbsModel] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BreadcrumbsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "breadcrumbsVM")
instance ToJSON BreadcrumbsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "breadcrumbsVM")


-- | 
data ClearNotificationsVM = ClearNotificationsVM
  { clearNotificationsVMUpdate :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ClearNotificationsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "clearNotificationsVM")
instance ToJSON ClearNotificationsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "clearNotificationsVM")


-- | 
data ContactGroupVM = ContactGroupVM
  { contactGroupVMId :: Maybe Text -- ^ 
  , contactGroupVMName :: Maybe Text -- ^ 
  , contactGroupVMSubscriptionId :: Maybe Text -- ^ 
  , contactGroupVMCreatedTime :: Maybe UTCTime -- ^ 
  , contactGroupVMCreatorUserId :: Maybe Text -- ^ 
  , contactGroupVMEditedTime :: Maybe UTCTime -- ^ 
  , contactGroupVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactGroupVM")
instance ToJSON ContactGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactGroupVM")


-- | 
data ContactGroupsVM = ContactGroupsVM
  { contactGroupsVMGroups :: Maybe [ContactGroupVM] -- ^ 
  , contactGroupsVMSkip :: Maybe Int -- ^ 
  , contactGroupsVMTake :: Maybe Int -- ^ 
  , contactGroupsVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactGroupsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactGroupsVM")
instance ToJSON ContactGroupsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactGroupsVM")


-- | 
data ContactVM = ContactVM
  { contactVMId :: Maybe Text -- ^ 
  , contactVMName :: Maybe Text -- ^ 
  , contactVMEmail :: Maybe Text -- ^ 
  , contactVMGroups :: Maybe [Text] -- ^ 
  , contactVMSubscriptionId :: Maybe Text -- ^ 
  , contactVMCreatedTime :: Maybe UTCTime -- ^ 
  , contactVMCreatorUserId :: Maybe Text -- ^ 
  , contactVMEditedTime :: Maybe UTCTime -- ^ 
  , contactVMEditorUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactVM")
instance ToJSON ContactVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactVM")


-- | 
data ContactsVM = ContactsVM
  { contactsVMContacts :: Maybe [ContactVM] -- ^ 
  , contactsVMSkip :: Maybe Int -- ^ 
  , contactsVMTake :: Maybe Int -- ^ 
  , contactsVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "contactsVM")
instance ToJSON ContactsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "contactsVM")


-- | 
data CountVM = CountVM
  { countVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CountVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "countVM")
instance ToJSON CountVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "countVM")


-- | 
data CreateApiKeyVM = CreateApiKeyVM
  { createApiKeyVMDescription :: Maybe Text -- ^ 
  , createApiKeyVMExpired :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createApiKeyVM")
instance ToJSON CreateApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createApiKeyVM")


-- | 
data CreateContactGroupVM = CreateContactGroupVM
  { createContactGroupVMName :: Text -- ^ 
  , createContactGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createContactGroupVM")
instance ToJSON CreateContactGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createContactGroupVM")


-- | 
data CreateContactVM = CreateContactVM
  { createContactVMName :: Maybe Text -- ^ 
  , createContactVMEmail :: Maybe Text -- ^ 
  , createContactVMGroups :: Maybe [Text] -- ^ 
  , createContactVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createContactVM")
instance ToJSON CreateContactVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createContactVM")


-- | 
newtype CreateDataSourceAdminVM = CreateDataSourceAdminVM { unCreateDataSourceAdminVM :: CreateDataSourceVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype CreateEmailTaskVM = CreateEmailTaskVM { unCreateEmailTaskVM :: CreateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateExportReportTaskVM = CreateExportReportTaskVM { unCreateExportReportTaskVM :: CreateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateExportTemplateTaskVM = CreateExportTemplateTaskVM { unCreateExportTemplateTaskVM :: CreateExportReportTaskVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateFTPUploadTaskVM = CreateFTPUploadTaskVM { unCreateFTPUploadTaskVM :: CreateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateFetchTaskVM = CreateFetchTaskVM { unCreateFetchTaskVM :: CreateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateGroupAdminVM = CreateGroupAdminVM { unCreateGroupAdminVM :: CreateGroupVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data CreateGroupVM = CreateGroupVM
  { createGroupVMName :: Text -- ^ 
  , createGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createGroupVM")
instance ToJSON CreateGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createGroupVM")


-- | 
newtype CreatePrepareTemplateTaskVM = CreatePrepareTemplateTaskVM { unCreatePrepareTemplateTaskVM :: CreateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
data CreateTaskBaseVM = CreateTaskBaseVM
  { createTaskBaseVMCronExpression :: Maybe Text -- ^ 
  , createTaskBaseVMDelayedRunTime :: Maybe UTCTime -- ^ 
  , createTaskBaseVMName :: Maybe Text -- ^ 
  , createTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , createTaskBaseVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createTaskBaseVM")
instance ToJSON CreateTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createTaskBaseVM")


-- | 
newtype CreateThumbnailReportTaskVM = CreateThumbnailReportTaskVM { unCreateThumbnailReportTaskVM :: CreateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateThumbnailTemplateTaskVM = CreateThumbnailTemplateTaskVM { unCreateThumbnailTemplateTaskVM :: CreateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateTransformTaskBaseVM = CreateTransformTaskBaseVM { unCreateTransformTaskBaseVM :: CreateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateTransportTaskBaseVM = CreateTransportTaskBaseVM { unCreateTransportTaskBaseVM :: CreateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateWebhookTaskVM = CreateWebhookTaskVM { unCreateWebhookTaskVM :: CreateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data DataSourceAdministrate = DataSourceAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceAdministrate")
instance ToJSON DataSourceAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceAdministrate")


-- | 
data DataSourceConnectionType = DataSourceConnectionType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceConnectionType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceConnectionType")
instance ToJSON DataSourceConnectionType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceConnectionType")


-- | 
data DataSourceCreate = DataSourceCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceCreate")
instance ToJSON DataSourceCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceCreate")


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


-- | 
data DataSourceDelete = DataSourceDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceDelete")
instance ToJSON DataSourceDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceDelete")


-- | 
data DataSourceExecute = DataSourceExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceExecute")
instance ToJSON DataSourceExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceExecute")


-- | 
data DataSourceGet = DataSourceGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceGet")
instance ToJSON DataSourceGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceGet")


-- | 
newtype DataSourcePermission = DataSourcePermission { unDataSourcePermission :: DataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermission }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype DataSourcePermissions = DataSourcePermissions { unDataSourcePermissions :: DataSourcePermissionDataSourceCreateDataSourceGetDataSourceUpdateDataSourceDeleteDataSourceExecuteDataSourceAdministratePermissions }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data DataSourcePermissionsVM = DataSourcePermissionsVM
  { dataSourcePermissionsVMPermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermissionsVM")
instance ToJSON DataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermissionsVM")


-- | 
data DataSourceSorting = DataSourceSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceSorting where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceSorting")
instance ToJSON DataSourceSorting where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceSorting")


-- | 
data DataSourceStatus = DataSourceStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceStatus")
instance ToJSON DataSourceStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceStatus")


-- | 
data DataSourceUpdate = DataSourceUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceUpdate")
instance ToJSON DataSourceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceUpdate")


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
  , dataSourceVMErrorMessage :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourceVM")
instance ToJSON DataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourceVM")


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


-- | 
data DefaultPermissionsVM = DefaultPermissionsVM
  { defaultPermissionsVMFilePermissions :: Maybe FilePermissions -- ^ 
  , defaultPermissionsVMDataSourcePermissions :: Maybe DataSourcePermissions -- ^ 
  , defaultPermissionsVMGroupPermissions :: Maybe GroupPermissions -- ^ 
  , defaultPermissionsVMTaskPermissions :: Maybe TaskPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DefaultPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "defaultPermissionsVM")
instance ToJSON DefaultPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "defaultPermissionsVM")


-- | 
data DeleteApiKeyVM = DeleteApiKeyVM
  { deleteApiKeyVMApiKey :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deleteApiKeyVM")
instance ToJSON DeleteApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deleteApiKeyVM")


-- | 
newtype EmailTaskVM = EmailTaskVM { unEmailTaskVM :: TransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data EntityType = EntityType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityType")
instance ToJSON EntityType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityType")


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


-- | 
newtype ExportCreateAdminVM = ExportCreateAdminVM { unExportCreateAdminVM :: ExportCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportCreateVM = ExportCreateVM { unExportCreateVM :: FileCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportFolderCreateVM = ExportFolderCreateVM { unExportFolderCreateVM :: FolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data ExportFormat = ExportFormat
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportFormat where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportFormat")
instance ToJSON ExportFormat where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportFormat")


-- | 
newtype ExportReportTaskVM = ExportReportTaskVM { unExportReportTaskVM :: TransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype ExportTemplateTaskVM = ExportTemplateTaskVM { unExportTemplateTaskVM :: ExportReportTaskVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype ExportVM = ExportVM { unExportVM :: FileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype ExportsVM = ExportsVM { unExportsVM :: ExportVMFilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FTPUploadTaskVM = FTPUploadTaskVM { unFTPUploadTaskVM :: TransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FetchTaskVM = FetchTaskVM { unFetchTaskVM :: TaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileAdministrate = FileAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileAdministrate")
instance ToJSON FileAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileAdministrate")


-- | 
data FileCreate = FileCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileCreate")
instance ToJSON FileCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileCreate")


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


-- | 
data FileDelete = FileDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileDelete")
instance ToJSON FileDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileDelete")


-- | 
data FileExecute = FileExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileExecute")
instance ToJSON FileExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileExecute")


-- | 
data FileGet = FileGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileGet")
instance ToJSON FileGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileGet")


-- | 
data FileIconVM = FileIconVM
  { fileIconVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileIconVM")
instance ToJSON FileIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileIconVM")


-- | 
data FileKind = FileKind
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileKind where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileKind")
instance ToJSON FileKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileKind")


-- | 
newtype FilePermission = FilePermission { unFilePermission :: FileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermission }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype FilePermissions = FilePermissions { unFilePermissions :: FilePermissionFileCreateFileGetFileUpdateFileDeleteFileExecuteFileAdministratePermissions }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FilePermissionsVM = FilePermissionsVM
  { filePermissionsVMPermissions :: Maybe FilePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FilePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filePermissionsVM")
instance ToJSON FilePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filePermissionsVM")


-- | 
data FileRenameVM = FileRenameVM
  { fileRenameVMName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileRenameVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileRenameVM")
instance ToJSON FileRenameVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileRenameVM")


-- | 
data FileSorting = FileSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileSorting where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileSorting")
instance ToJSON FileSorting where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileSorting")


-- | 
data FileStatus = FileStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatus where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileStatus")
instance ToJSON FileStatus where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileStatus")


-- | 
data FileStatusReason = FileStatusReason
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatusReason where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileStatusReason")
instance ToJSON FileStatusReason where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileStatusReason")


-- | 
data FileTagsUpdateVM = FileTagsUpdateVM
  { fileTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileTagsUpdateVM")
instance ToJSON FileTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileTagsUpdateVM")


-- | 
data FileType = FileType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileType")
instance ToJSON FileType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileType")


-- | 
data FileUpdate = FileUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileUpdate")
instance ToJSON FileUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileUpdate")


-- | 
newtype FileVM = FileVM { unFileVM :: EntityVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype FilesVM = FilesVM { unFilesVM :: FileVMFilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
data FolderIconVM = FolderIconVM
  { folderIconVMIcon :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderIconVM")
instance ToJSON FolderIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderIconVM")


-- | 
data FolderRenameVM = FolderRenameVM
  { folderRenameVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderRenameVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderRenameVM")
instance ToJSON FolderRenameVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderRenameVM")


-- | 
data FolderSizeVM = FolderSizeVM
  { folderSizeVMSize :: Maybe Integer -- ^ 
  , folderSizeVMRealSize :: Maybe Integer -- ^ 
  , folderSizeVMDocumentsCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderSizeVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderSizeVM")
instance ToJSON FolderSizeVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderSizeVM")


-- | 
data FolderTagsUpdateVM = FolderTagsUpdateVM
  { folderTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderTagsUpdateVM")
instance ToJSON FolderTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderTagsUpdateVM")


-- | 
data FrontendApp = FrontendApp
  { frontendAppMixins :: Maybe AppMixins -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FrontendApp where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "frontendApp")
instance ToJSON FrontendApp where
  toJSON = genericToJSON (removeFieldLabelPrefix False "frontendApp")


-- | 
data GroupAdministrate = GroupAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupAdministrate")
instance ToJSON GroupAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupAdministrate")


-- | 
data GroupCreate = GroupCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupCreate")
instance ToJSON GroupCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupCreate")


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


-- | 
data GroupDelete = GroupDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupDelete")
instance ToJSON GroupDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupDelete")


-- | 
data GroupExecute = GroupExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupExecute")
instance ToJSON GroupExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupExecute")


-- | 
data GroupGet = GroupGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupGet")
instance ToJSON GroupGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupGet")


-- | 
newtype GroupPermission = GroupPermission { unGroupPermission :: GroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermission }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype GroupPermissions = GroupPermissions { unGroupPermissions :: GroupPermissionGroupCreateGroupGetGroupUpdateGroupDeleteGroupExecuteGroupAdministratePermissions }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data GroupPermissionsVM = GroupPermissionsVM
  { groupPermissionsVMPermissions :: Maybe GroupPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermissionsVM")
instance ToJSON GroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermissionsVM")


-- | 
data GroupUpdate = GroupUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupUpdate")
instance ToJSON GroupUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupUpdate")


-- | 
data GroupUserVM = GroupUserVM
  { groupUserVMUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupUserVM")
instance ToJSON GroupUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupUserVM")


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


-- | 
newtype HttpValidationProblemDetails = HttpValidationProblemDetails { unHttpValidationProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data InputFileVM = InputFileVM
  { inputFileVMEntityId :: Maybe Text -- ^ 
  , inputFileVMType :: Maybe FileKind -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InputFileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inputFileVM")
instance ToJSON InputFileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inputFileVM")


-- | 
data InvitedUser = InvitedUser
  { invitedUserUserId :: Maybe Text -- ^ 
  , invitedUserInvitedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InvitedUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "invitedUser")
instance ToJSON InvitedUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "invitedUser")


-- | 
data MyPermissionsVM = MyPermissionsVM
  { myPermissionsVMSubscription :: Maybe SubscriptionPermission -- ^ 
  , myPermissionsVMFiles :: Maybe FilePermission -- ^ 
  , myPermissionsVMDatasources :: Maybe DataSourcePermission -- ^ 
  , myPermissionsVMGroups :: Maybe GroupPermission -- ^ 
  , myPermissionsVMTasks :: Maybe TaskPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON MyPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "myPermissionsVM")
instance ToJSON MyPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "myPermissionsVM")


-- | 
data OutputFileVM = OutputFileVM
  { outputFileVMFileName :: Maybe Text -- ^ 
  , outputFileVMFolderId :: Maybe Text -- ^ 
  , outputFileVMType :: Maybe FileKind -- ^ 
  , outputFileVMIsTemporary :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON OutputFileVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "outputFileVM")
instance ToJSON OutputFileVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "outputFileVM")


-- | 
newtype PrepareTemplateTaskVM = PrepareTemplateTaskVM { unPrepareTemplateTaskVM :: TransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data PrepareTemplateVM = PrepareTemplateVM
  { prepareTemplateVMName :: Maybe Text -- ^ 
  , prepareTemplateVMLocale :: Maybe Text -- ^ 
  , prepareTemplateVMFolderId :: Maybe Text -- ^ 
  , prepareTemplateVMPagesCount :: Maybe Int -- ^ 
  , prepareTemplateVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PrepareTemplateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "prepareTemplateVM")
instance ToJSON PrepareTemplateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "prepareTemplateVM")


-- | 
data PreviewReportVM = PreviewReportVM
  { previewReportVMLocale :: Maybe Text -- ^ 
  , previewReportVMCacheTolerance :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PreviewReportVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "previewReportVM")
instance ToJSON PreviewReportVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "previewReportVM")


-- | 
data PreviewTemplateVM = PreviewTemplateVM
  { previewTemplateVMLocale :: Maybe Text -- ^ 
  , previewTemplateVMReportParameters :: Maybe (Map.Map String Text) -- ^ 
  , previewTemplateVMCacheTolerance :: Maybe Double -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PreviewTemplateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "previewTemplateVM")
instance ToJSON PreviewTemplateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "previewTemplateVM")


-- | 
newtype ProblemDetails = ProblemDetails { unProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data ProfileVisibility = ProfileVisibility
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProfileVisibility where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "profileVisibility")
instance ToJSON ProfileVisibility where
  toJSON = genericToJSON (removeFieldLabelPrefix False "profileVisibility")


-- | 
data RenameDataSourceVM = RenameDataSourceVM
  { renameDataSourceVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameDataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameDataSourceVM")
instance ToJSON RenameDataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameDataSourceVM")


-- | 
data RenameGroupVM = RenameGroupVM
  { renameGroupVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameGroupVM")
instance ToJSON RenameGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameGroupVM")


-- | 
data RenameSubscriptionVM = RenameSubscriptionVM
  { renameSubscriptionVMName :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameSubscriptionVM")
instance ToJSON RenameSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameSubscriptionVM")


-- | 
newtype ReportCreateAdminVM = ReportCreateAdminVM { unReportCreateAdminVM :: ReportCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ReportCreateVM = ReportCreateVM { unReportCreateVM :: FileCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ReportFolderCreateVM = ReportFolderCreateVM { unReportFolderCreateVM :: FolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype ReportVM = ReportVM { unReportVM :: FileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype ReportsVM = ReportsVM { unReportsVM :: ReportVMFilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunEmailTaskVM = RunEmailTaskVM { unRunEmailTaskVM :: RunTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunExportReportTaskVM = RunExportReportTaskVM { unRunExportReportTaskVM :: RunTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunExportTemplateTaskVM = RunExportTemplateTaskVM { unRunExportTemplateTaskVM :: RunExportReportTaskVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunFTPUploadTaskVM = RunFTPUploadTaskVM { unRunFTPUploadTaskVM :: RunTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunFetchTaskVM = RunFetchTaskVM { unRunFetchTaskVM :: RunTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunInputFileVM = RunInputFileVM { unRunInputFileVM :: InputFileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunPrepareTemplateTaskVM = RunPrepareTemplateTaskVM { unRunPrepareTemplateTaskVM :: RunTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data RunTaskBaseVM = RunTaskBaseVM
  { runTaskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , runTaskBaseVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RunTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "runTaskBaseVM")
instance ToJSON RunTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "runTaskBaseVM")


-- | 
newtype RunThumbnailReportTaskVM = RunThumbnailReportTaskVM { unRunThumbnailReportTaskVM :: RunTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunThumbnailTemplateTaskVM = RunThumbnailTemplateTaskVM { unRunThumbnailTemplateTaskVM :: RunTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunTransformTaskBaseVM = RunTransformTaskBaseVM { unRunTransformTaskBaseVM :: RunTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunTransportTaskBaseVM = RunTransportTaskBaseVM { unRunTransportTaskBaseVM :: RunTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RunWebhookTaskVM = RunWebhookTaskVM { unRunWebhookTaskVM :: RunTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data SaveMode = SaveMode
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SaveMode where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "saveMode")
instance ToJSON SaveMode where
  toJSON = genericToJSON (removeFieldLabelPrefix False "saveMode")


-- | 
newtype SelectedFilesForDeletingVM = SelectedFilesForDeletingVM { unSelectedFilesForDeletingVM :: SelectedFilesVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data SelectedFilesVM = SelectedFilesVM
  { selectedFilesVMIsAllSelected :: Maybe Bool -- ^ 
  , selectedFilesVMFiles :: Maybe [Text] -- ^ 
  , selectedFilesVMFolders :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SelectedFilesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "selectedFilesVM")
instance ToJSON SelectedFilesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "selectedFilesVM")


-- | 
data ServerConfigurationVM = ServerConfigurationVM
  { serverConfigurationVMTitle :: Maybe Text -- ^ 
  , serverConfigurationVMLogoLink :: Maybe Text -- ^ 
  , serverConfigurationVMCopyright :: Maybe Text -- ^ 
  , serverConfigurationVMCorporateServerMode :: Maybe Bool -- ^ 
  , serverConfigurationVMLastSLAVersion :: Maybe UTCTime -- ^ 
  , serverConfigurationVMIsDisabled :: Maybe Bool -- ^ 
  , serverConfigurationVMFrontend :: Maybe FrontendApp -- ^ 
  , serverConfigurationVMInvariantLocale :: Maybe Text -- ^ 
  , serverConfigurationVMAuth :: Maybe AuthConfigVM -- ^ 
  , serverConfigurationVMDesignerForAnons :: Maybe Bool -- ^ 
  , serverConfigurationVMSlaLink :: Maybe Text -- ^ 
  , serverConfigurationVMFirstStepsVideoLink :: Maybe Text -- ^ 
  , serverConfigurationVMAboutLink :: Maybe Text -- ^ 
  , serverConfigurationVMHomePageLink :: Maybe Text -- ^ 
  , serverConfigurationVMAuthServerName :: Maybe Text -- ^ 
  , serverConfigurationVMUpdateWorkspaceLink :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ServerConfigurationVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serverConfigurationVM")
instance ToJSON ServerConfigurationVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serverConfigurationVM")


-- | 
data SubscriptionAdministrate = SubscriptionAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionAdministrate")
instance ToJSON SubscriptionAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionAdministrate")


-- | 
data SubscriptionCreate = SubscriptionCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionCreate")
instance ToJSON SubscriptionCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionCreate")


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


-- | 
data SubscriptionDelete = SubscriptionDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionDelete")
instance ToJSON SubscriptionDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionDelete")


-- | 
data SubscriptionExecute = SubscriptionExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionExecute")
instance ToJSON SubscriptionExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionExecute")


-- | 
data SubscriptionFolder = SubscriptionFolder
  { subscriptionFolderFolderId :: Maybe Text -- ^ 
  , subscriptionFolderBytesUsed :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionFolder where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionFolder")
instance ToJSON SubscriptionFolder where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionFolder")


-- | 
data SubscriptionGet = SubscriptionGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionGet")
instance ToJSON SubscriptionGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionGet")


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


-- | 
data SubscriptionInvitesVM = SubscriptionInvitesVM
  { subscriptionInvitesVMInvites :: Maybe [SubscriptionInviteVM] -- ^ 
  , subscriptionInvitesVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionInvitesVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionInvitesVM")
instance ToJSON SubscriptionInvitesVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionInvitesVM")


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


-- | 
newtype SubscriptionPermission = SubscriptionPermission { unSubscriptionPermission :: SubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermission }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype SubscriptionPermissions = SubscriptionPermissions { unSubscriptionPermissions :: SubscriptionPermissionSubscriptionCreateSubscriptionGetSubscriptionUpdateSubscriptionDeleteSubscriptionExecuteSubscriptionAdministratePermissions }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data SubscriptionPermissionsVM = SubscriptionPermissionsVM
  { subscriptionPermissionsVMPermissions :: Maybe SubscriptionPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionPermissionsVM")
instance ToJSON SubscriptionPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionPermissionsVM")


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


-- | 
data SubscriptionUpdate = SubscriptionUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionUpdate")
instance ToJSON SubscriptionUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionUpdate")


-- | 
data SubscriptionUserVM = SubscriptionUserVM
  { subscriptionUserVMUserId :: Maybe Text -- ^ 
  , subscriptionUserVMGroups :: Maybe [GroupVM] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "subscriptionUserVM")
instance ToJSON SubscriptionUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "subscriptionUserVM")


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


-- | 
data TaskAdministrate = TaskAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskAdministrate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskAdministrate")
instance ToJSON TaskAdministrate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskAdministrate")


-- | 
data TaskBaseVM = TaskBaseVM
  { taskBaseVMCronExpression :: Maybe Text -- ^ 
  , taskBaseVMDelayedRunTime :: Maybe UTCTime -- ^ 
  , taskBaseVMDelayedWasRunTime :: Maybe UTCTime -- ^ 
  , taskBaseVMId :: Maybe Text -- ^ 
  , taskBaseVMName :: Maybe Text -- ^ 
  , taskBaseVMRecurrentRunTime :: Maybe UTCTime -- ^ 
  , taskBaseVMRecurrentWasRunTime :: Maybe UTCTime -- ^ 
  , taskBaseVMSubscriptionId :: Maybe Text -- ^ 
  , taskBaseVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskBaseVM")
instance ToJSON TaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskBaseVM")


-- | 
data TaskCreate = TaskCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskCreate")
instance ToJSON TaskCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskCreate")


-- | 
data TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission = TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission
  { taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionCreate :: Maybe TaskCreate -- ^ 
  , taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionDelete :: Maybe TaskDelete -- ^ 
  , taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionExecute :: Maybe TaskExecute -- ^ 
  , taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionGet :: Maybe TaskGet -- ^ 
  , taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionUpdate :: Maybe TaskUpdate -- ^ 
  , taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionAdministrate :: Maybe TaskAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission")
instance ToJSON TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission")


-- | 
data TaskDelete = TaskDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskDelete where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskDelete")
instance ToJSON TaskDelete where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskDelete")


-- | 
data TaskExecute = TaskExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskExecute")
instance ToJSON TaskExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskExecute")


-- | 
data TaskGet = TaskGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskGet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskGet")
instance ToJSON TaskGet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskGet")


-- | 
newtype TaskPermission = TaskPermission { unTaskPermission :: TaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermission }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions = TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions
  { taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionsOwnerId :: Maybe Text -- ^ 
  , taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionsOwner :: Maybe TaskPermission -- ^ 
  , taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionsGroups :: Maybe (Map.Map String TaskPermission) -- ^ 
  , taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionsOther :: Maybe TaskPermission -- ^ 
  , taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissionsAnon :: Maybe TaskPermission -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions")
instance ToJSON TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions")


-- | 
newtype TaskPermissions = TaskPermissions { unTaskPermissions :: TaskPermissionTaskCreateTaskGetTaskUpdateTaskDeleteTaskExecuteTaskAdministratePermissions }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TaskPermissionsVM = TaskPermissionsVM
  { taskPermissionsVMPermissions :: Maybe TaskPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskPermissionsVM")
instance ToJSON TaskPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskPermissionsVM")


-- | 
data TaskSettingsVM = TaskSettingsVM
  { taskSettingsVMPrepare :: Maybe Bool -- ^ 
  , taskSettingsVMExportTemplate :: Maybe Bool -- ^ 
  , taskSettingsVMExportReport :: Maybe Bool -- ^ 
  , taskSettingsVMSendViaEmail :: Maybe Bool -- ^ 
  , taskSettingsVMUploadToFTP :: Maybe Bool -- ^ 
  , taskSettingsVMSendViaWebhook :: Maybe Bool -- ^ 
  , taskSettingsVMFetchData :: Maybe Bool -- ^ 
  , taskSettingsVMThumbnailReport :: Maybe Bool -- ^ 
  , taskSettingsVMThumbnailTemplate :: Maybe Bool -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskSettingsVM")
instance ToJSON TaskSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskSettingsVM")


-- | 
data TaskUpdate = TaskUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "taskUpdate")
instance ToJSON TaskUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "taskUpdate")


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


-- | 
newtype TemplateCreateAdminVM = TemplateCreateAdminVM { unTemplateCreateAdminVM :: TemplateCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateCreateVM = TemplateCreateVM { unTemplateCreateVM :: FileCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateFolderCreateVM = TemplateFolderCreateVM { unTemplateFolderCreateVM :: FolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateVM = TemplateVM { unTemplateVM :: FileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
newtype TemplatesVM = TemplatesVM { unTemplatesVM :: TemplateVMFilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ThumbnailReportTaskVM = ThumbnailReportTaskVM { unThumbnailReportTaskVM :: TaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ThumbnailTemplateTaskVM = ThumbnailTemplateTaskVM { unThumbnailTemplateTaskVM :: TaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TimePeriodType = TimePeriodType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TimePeriodType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "timePeriodType")
instance ToJSON TimePeriodType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "timePeriodType")


-- | 
newtype TransformTaskBaseVM = TransformTaskBaseVM { unTransformTaskBaseVM :: TaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TransportTaskBaseVM = TransportTaskBaseVM { unTransportTaskBaseVM :: TaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data UpdateContactGroupVM = UpdateContactGroupVM
  { updateContactGroupVMName :: Text -- ^ 
  , updateContactGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateContactGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateContactGroupVM")
instance ToJSON UpdateContactGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateContactGroupVM")


-- | 
data UpdateContactVM = UpdateContactVM
  { updateContactVMName :: Maybe Text -- ^ 
  , updateContactVMEmail :: Maybe Text -- ^ 
  , updateContactVMGroups :: Maybe [Text] -- ^ 
  , updateContactVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateContactVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateContactVM")
instance ToJSON UpdateContactVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateContactVM")


-- | 
data UpdateDataSourceConnectionStringVM = UpdateDataSourceConnectionStringVM
  { updateDataSourceConnectionStringVMConnectionString :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceConnectionStringVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceConnectionStringVM")
instance ToJSON UpdateDataSourceConnectionStringVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceConnectionStringVM")


-- | 
data UpdateDataSourcePermissionsVM = UpdateDataSourcePermissionsVM
  { updateDataSourcePermissionsVMNewPermissions :: DataSourcePermissions -- ^ 
  , updateDataSourcePermissionsVMAdministrate :: DataSourceAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourcePermissionsVM")
instance ToJSON UpdateDataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourcePermissionsVM")


-- | 
data UpdateDataSourceSubscriptionVM = UpdateDataSourceSubscriptionVM
  { updateDataSourceSubscriptionVMSubscriptionId :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceSubscriptionVM")
instance ToJSON UpdateDataSourceSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceSubscriptionVM")


-- | 
data UpdateDefaultPermissionsVM = UpdateDefaultPermissionsVM
  { updateDefaultPermissionsVMFilePermissions :: Maybe UpdateFilePermissionsVM -- ^ 
  , updateDefaultPermissionsVMGroupPermissions :: Maybe UpdateGroupPermissionsVM -- ^ 
  , updateDefaultPermissionsVMDataSourcePermissions :: Maybe UpdateDataSourcePermissionsVM -- ^ 
  , updateDefaultPermissionsVMTaskPermissions :: Maybe UpdateTaskPermissionsVM -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDefaultPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDefaultPermissionsVM")
instance ToJSON UpdateDefaultPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDefaultPermissionsVM")


-- | 
newtype UpdateEmailTaskVM = UpdateEmailTaskVM { unUpdateEmailTaskVM :: UpdateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateExportReportTaskVM = UpdateExportReportTaskVM { unUpdateExportReportTaskVM :: UpdateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateExportTemplateTaskVM = UpdateExportTemplateTaskVM { unUpdateExportTemplateTaskVM :: UpdateExportReportTaskVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateFTPUploadTaskVM = UpdateFTPUploadTaskVM { unUpdateFTPUploadTaskVM :: UpdateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateFetchTaskVM = UpdateFetchTaskVM { unUpdateFetchTaskVM :: UpdateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data UpdateFileContentVM = UpdateFileContentVM
  { updateFileContentVMContent :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateFileContentVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateFileContentVM")
instance ToJSON UpdateFileContentVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateFileContentVM")


-- | 
data UpdateFilePermissionsVM = UpdateFilePermissionsVM
  { updateFilePermissionsVMNewPermissions :: FilePermissions -- ^ 
  , updateFilePermissionsVMAdministrate :: FileAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateFilePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateFilePermissionsVM")
instance ToJSON UpdateFilePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateFilePermissionsVM")


-- | 
data UpdateGroupPermissionsVM = UpdateGroupPermissionsVM
  { updateGroupPermissionsVMNewPermissions :: GroupPermissions -- ^ 
  , updateGroupPermissionsVMAdministrate :: GroupAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupPermissionsVM")
instance ToJSON UpdateGroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupPermissionsVM")


-- | 
newtype UpdatePrepareTemplateTaskVM = UpdatePrepareTemplateTaskVM { unUpdatePrepareTemplateTaskVM :: UpdateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data UpdateSubscriptionLocaleVM = UpdateSubscriptionLocaleVM
  { updateSubscriptionLocaleVMLocale :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionLocaleVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionLocaleVM")
instance ToJSON UpdateSubscriptionLocaleVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionLocaleVM")


-- | 
data UpdateSubscriptionPermissionsVM = UpdateSubscriptionPermissionsVM
  { updateSubscriptionPermissionsVMNewPermissions :: SubscriptionPermissions -- ^ 
  , updateSubscriptionPermissionsVMAdministrate :: SubscriptionAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionPermissionsVM")
instance ToJSON UpdateSubscriptionPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionPermissionsVM")


-- | 
data UpdateTaskBaseVM = UpdateTaskBaseVM
  { updateTaskBaseVMCronExpression :: Maybe Text -- ^ 
  , updateTaskBaseVMDelayedRunTime :: Maybe UTCTime -- ^ 
  , updateTaskBaseVMName :: Maybe Text -- ^ 
  , updateTaskBaseVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateTaskBaseVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateTaskBaseVM")
instance ToJSON UpdateTaskBaseVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateTaskBaseVM")


-- | 
data UpdateTaskPermissionsVM = UpdateTaskPermissionsVM
  { updateTaskPermissionsVMAdministrate :: TaskAdministrate -- ^ 
  , updateTaskPermissionsVMNewPermissions :: TaskPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateTaskPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateTaskPermissionsVM")
instance ToJSON UpdateTaskPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateTaskPermissionsVM")


-- | 
newtype UpdateThumbnailReportTaskVM = UpdateThumbnailReportTaskVM { unUpdateThumbnailReportTaskVM :: UpdateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateThumbnailTemplateTaskVM = UpdateThumbnailTemplateTaskVM { unUpdateThumbnailTemplateTaskVM :: UpdateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateTransformTaskBaseVM = UpdateTransformTaskBaseVM { unUpdateTransformTaskBaseVM :: UpdateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateTransportTaskBaseVM = UpdateTransportTaskBaseVM { unUpdateTransportTaskBaseVM :: UpdateTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
data UpdateUserSettingsVM = UpdateUserSettingsVM
  { updateUserSettingsVMProfileVisibility :: Maybe ProfileVisibility -- ^ 
  , updateUserSettingsVMDefaultSubscription :: Maybe Text -- ^ 
  , updateUserSettingsVMShowHiddenFilesAndFolders :: Maybe Bool -- ^ 
  , updateUserSettingsVMSubscribedNotifications :: Maybe [AuditType] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateUserSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUserSettingsVM")
instance ToJSON UpdateUserSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUserSettingsVM")


-- | 
newtype UpdateWebhookTaskVM = UpdateWebhookTaskVM { unUpdateWebhookTaskVM :: UpdateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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


-- | 
data UserSettingsVM = UserSettingsVM
  { userSettingsVMProfileVisibility :: Maybe ProfileVisibility -- ^ 
  , userSettingsVMDefaultSubscription :: Maybe Text -- ^ 
  , userSettingsVMShowHiddenFilesAndFolders :: Maybe Bool -- ^ 
  , userSettingsVMSlaAcceptedDateTime :: Maybe UTCTime -- ^ 
  , userSettingsVMSubscribedNotifications :: Maybe [AuditType] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserSettingsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userSettingsVM")
instance ToJSON UserSettingsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userSettingsVM")


-- | 
newtype WebhookTaskVM = WebhookTaskVM { unWebhookTaskVM :: TransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

uncapitalize :: String -> String
uncapitalize (first:rest) = Char.toLower first : rest
uncapitalize [] = []

-- | Remove a field label prefix during JSON parsing.
--   Also perform any replacements for special characters.
--   The @forParsing@ parameter is to distinguish between the cases in which we're using this
--   to power a @FromJSON@ or a @ToJSON@ instance. In the first case we're parsing, and we want
--   to replace special characters with their quoted equivalents (because we cannot have special
--   chars in identifier names), while we want to do vice versa when sending data instead.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = uncapitalize . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("$", "'Dollar")
      , ("^", "'Caret")
      , ("|", "'Pipe")
      , ("=", "'Equal")
      , ("*", "'Star")
      , ("-", "'Dash")
      , ("&", "'Ampersand")
      , ("%", "'Percent")
      , ("#", "'Hash")
      , ("@", "'At")
      , ("!", "'Exclamation")
      , ("+", "'Plus")
      , (":", "'Colon")
      , (";", "'Semicolon")
      , (">", "'GreaterThan")
      , ("<", "'LessThan")
      , (".", "'Period")
      , ("_", "'Underscore")
      , ("?", "'Question_Mark")
      , (",", "'Comma")
      , ("'", "'Quote")
      , ("/", "'Slash")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("{", "'Left_Curly_Bracket")
      , ("}", "'Right_Curly_Bracket")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("~", "'Tilde")
      , ("`", "'Backtick")
      , ("<=", "'Less_Than_Or_Equal_To")
      , (">=", "'Greater_Than_Or_Equal_To")
      , ("!=", "'Not_Equal")
      , ("<>", "'Not_Equal")
      , ("~=", "'Tilde_Equal")
      , ("\\", "'Back_Slash")
      , ("\"", "'Double_Quote")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
