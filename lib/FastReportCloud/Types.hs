{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module FastReportCloud.Types (
  AcceptAgreementsVM (..),
  AdminAdministrate (..),
  AdminCreate (..),
  AdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission (..),
  AdminDelete (..),
  AdminExecute (..),
  AdminExportFolderCreateVM (..),
  AdminFolderCreateVM (..),
  AdminGet (..),
  AdminPermission (..),
  AdminReportFolderCreateVM (..),
  AdminSubscriptionVM (..),
  AdminSubscriptionsVM (..),
  AdminTemplateFolderCreateVM (..),
  AdminUpdate (..),
  AdminUpdateCurrentSubscriptionPlanVM (..),
  AnalysisResultVM (..),
  AnalysisResultsVM (..),
  ApiKeyVM (..),
  ApiKeysVM (..),
  AppMixinsVM (..),
  AuditActionVM (..),
  AuditActionsVM (..),
  AuditActiveStatsVM (..),
  AuditFilePropertyChangedVM (..),
  AuditStatVM (..),
  AuditStatsVM (..),
  AuditSubscriptionActionVM (..),
  AuditTaskActionVM (..),
  AuditType (..),
  AuthConfigVM (..),
  BreadcrumbsModel (..),
  BreadcrumbsVM (..),
  CheckUserByPasswordInternalVM (..),
  ClearNotificationsVM (..),
  CloudBaseVM (..),
  ContactGroupVM (..),
  ContactGroupsVM (..),
  ContactVM (..),
  ContactsVM (..),
  CountVM (..),
  CreateApiKeyVM (..),
  CreateAuditActionVM (..),
  CreateContactGroupVM (..),
  CreateContactVM (..),
  CreateDataSourceAdminVM (..),
  CreateDataSourceVM (..),
  CreateEmailTaskVM (..),
  CreateExportReportTaskVM (..),
  CreateExportTemplateTaskVM (..),
  CreateFTPUploadTaskVM (..),
  CreateFetchTaskVM (..),
  CreateFileShareVM (..),
  CreateGroupAdminVM (..),
  CreateGroupVM (..),
  CreateIfNotExistInternalVM (..),
  CreatePrepareTemplateTaskVM (..),
  CreateSubscriptionInviteVM (..),
  CreateSubscriptionPeriodVM (..),
  CreateSubscriptionPlanVM (..),
  CreateSubscriptionVM (..),
  CreateTaskBaseVM (..),
  CreateTaskEndVM (..),
  CreateThumbnailReportTaskVM (..),
  CreateThumbnailTemplateTaskVM (..),
  CreateTransformTaskBaseVM (..),
  CreateTransportTaskBaseVM (..),
  CreateWebhookTaskVM (..),
  DataSourceAdministrate (..),
  DataSourceConnectionType (..),
  DataSourceCreate (..),
  DataSourceDelete (..),
  DataSourceExecute (..),
  DataSourceGet (..),
  DataSourceParameterTypeVM (..),
  DataSourceParameterTypesVM (..),
  DataSourcePermissionCRUDVM (..),
  DataSourcePermissionsCRUDVM (..),
  DataSourcePermissionsVM (..),
  DataSourceSelectCommandParameterVM (..),
  DataSourceSelectCommandVM (..),
  DataSourceSorting (..),
  DataSourceStatus (..),
  DataSourceUpdate (..),
  DataSourceVM (..),
  DataSourcesVM (..),
  DefaultPermissionsVM (..),
  DeleteApiKeyVM (..),
  EmailTaskVM (..),
  EntityType (..),
  ExportCreateAdminVM (..),
  ExportCreateVM (..),
  ExportFolderCreateVM (..),
  ExportFormat (..),
  ExportReportTaskVM (..),
  ExportReportVM (..),
  ExportTemplateTaskVM (..),
  ExportTemplateVM (..),
  ExportVM (..),
  ExportsVM (..),
  FTPUploadTaskVM (..),
  FetchTaskVM (..),
  FileAdministrate (..),
  FileContentVM (..),
  FileCreate (..),
  FileCreateFormVM (..),
  FileCreateVM (..),
  FileDelete (..),
  FileExecute (..),
  FileGet (..),
  FileIconVM (..),
  FileKind (..),
  FilePermissionCRUDVM (..),
  FilePermissionsCRUDVM (..),
  FilePermissionsVM (..),
  FileRenameVM (..),
  FileShareVM (..),
  FileSharingKeysVM (..),
  FileSorting (..),
  FileStatus (..),
  FileStatusReason (..),
  FileStatusUpdateInternalVM (..),
  FileStatusVM (..),
  FileTagsUpdateVM (..),
  FileThumbnailUpdateInternalVM (..),
  FileType (..),
  FileUpdate (..),
  FileUpdateVM (..),
  FileVM (..),
  FilesVM (..),
  FilesVMBase (..),
  FolderCreateVM (..),
  FolderIconVM (..),
  FolderRenameVM (..),
  FolderSizeVM (..),
  FolderTagsUpdateVM (..),
  FrontendAppVM (..),
  GroupAdministrate (..),
  GroupCreate (..),
  GroupDelete (..),
  GroupExecute (..),
  GroupGet (..),
  GroupPermissionCRUDVM (..),
  GroupPermissionsCRUDVM (..),
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
  ProblemLevel (..),
  ProblemType (..),
  ProfileVisibility (..),
  RegisterUserVM (..),
  RenameDataSourceVM (..),
  RenameGroupVM (..),
  RenameSubscriptionVM (..),
  ReportCreateAdminVM (..),
  ReportCreateFormVM (..),
  ReportCreateVM (..),
  ReportFolderCreateVM (..),
  ReportInfo (..),
  ReportVM (..),
  ReportsVM (..),
  RestOfSpaceVM (..),
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
  SelectedFilesVM (..),
  ServerConfigurationVM (..),
  SolvationReportVM (..),
  SubscriptionAdministrate (..),
  SubscriptionCreate (..),
  SubscriptionDelete (..),
  SubscriptionExecute (..),
  SubscriptionFolder (..),
  SubscriptionGet (..),
  SubscriptionInviteVM (..),
  SubscriptionInvitesVM (..),
  SubscriptionPeriodVM (..),
  SubscriptionPermissionCRUDVM (..),
  SubscriptionPermissionsCRUDVM (..),
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
  TaskDelete (..),
  TaskEnd (..),
  TaskExecute (..),
  TaskGet (..),
  TaskIdsVM (..),
  TaskMessageIdVM (..),
  TaskPermissionCRUDVM (..),
  TaskPermissionsCRUDVM (..),
  TaskPermissionsVM (..),
  TaskSettingsVM (..),
  TaskUpdate (..),
  TasksVM (..),
  TemplateContentVM (..),
  TemplateCreateAdminVM (..),
  TemplateCreateFormVM (..),
  TemplateCreateVM (..),
  TemplateFolderCreateVM (..),
  TemplateVM (..),
  TemplatesVM (..),
  ThumbnailReportTaskVM (..),
  ThumbnailTemplateTaskVM (..),
  TimePeriodType (..),
  TransformTaskBaseVM (..),
  TransportTaskBaseVM (..),
  UpdateContactGroupVM (..),
  UpdateContactVM (..),
  UpdateContentInternalVM (..),
  UpdateDataSourceConnectionStringVM (..),
  UpdateDataSourcePermissionsVM (..),
  UpdateDataSourceSelectCommandsVM (..),
  UpdateDataSourceSubscriptionVM (..),
  UpdateDataSourceVM (..),
  UpdateDataVM (..),
  UpdateDefaultPermissionsVM (..),
  UpdateEmailTaskVM (..),
  UpdateExportReportTaskVM (..),
  UpdateExportTemplateTaskVM (..),
  UpdateFTPUploadTaskVM (..),
  UpdateFetchTaskVM (..),
  UpdateFileContentFormVM (..),
  UpdateFileContentInternalVM (..),
  UpdateFileContentVM (..),
  UpdateFilePermissionsVM (..),
  UpdateGroupPermissionsVM (..),
  UpdateGroupVM (..),
  UpdatePrepareTemplateTaskVM (..),
  UpdateSubscriptionLocaleVM (..),
  UpdateSubscriptionPermissionsVM (..),
  UpdateSubscriptionPlanVM (..),
  UpdateSubscriptionVM (..),
  UpdateTaskBaseVM (..),
  UpdateTaskPermissionsVM (..),
  UpdateThumbnailReportTaskVM (..),
  UpdateThumbnailTemplateTaskVM (..),
  UpdateTransformTaskBaseVM (..),
  UpdateTransportTaskBaseVM (..),
  UpdateUserProfileVM (..),
  UpdateUserSettingsVM (..),
  UpdateUserVM (..),
  UpdateWebhookTaskVM (..),
  UserIsAdminVM (..),
  UserProfileVM (..),
  UserResultVM (..),
  UserSettingsVM (..),
  UserVM (..),
  UsersVM (..),
  WebhookTaskVM (..),
  ) where

import Data.Data (Data)
import Data.UUID (UUID)
import Data.List (lookup)
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


-- | 
newtype AcceptAgreementsVM = AcceptAgreementsVM { unAcceptAgreementsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data AdminAdministrate = AdminAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminAdministrate where
  parseJSON = genericParseJSON optionsAdminAdministrate
instance ToJSON AdminAdministrate where
  toJSON = genericToJSON optionsAdminAdministrate

optionsAdminAdministrate :: Options
optionsAdminAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data AdminCreate = AdminCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreate where
  parseJSON = genericParseJSON optionsAdminCreate
instance ToJSON AdminCreate where
  toJSON = genericToJSON optionsAdminCreate

optionsAdminCreate :: Options
optionsAdminCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data AdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission = AdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission
  { adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionCreate :: Maybe AdminCreate -- ^ 
  , adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionDelete :: Maybe AdminDelete -- ^ 
  , adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionExecute :: Maybe AdminExecute -- ^ 
  , adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionGet :: Maybe AdminGet -- ^ 
  , adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionUpdate :: Maybe AdminUpdate -- ^ 
  , adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionAdministrate :: Maybe AdminAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission where
  parseJSON = genericParseJSON optionsAdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission
instance ToJSON AdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission where
  toJSON = genericToJSON optionsAdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission

optionsAdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission :: Options
optionsAdminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermission =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionCreate", "create")
      , ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionDelete", "delete")
      , ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionExecute", "execute")
      , ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionGet", "get")
      , ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionUpdate", "update")
      , ("adminCreateAdminGetAdminUpdateAdminDeleteAdminExecuteAdminAdministratePermissionAdministrate", "administrate")
      ]


-- | 
data AdminDelete = AdminDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminDelete where
  parseJSON = genericParseJSON optionsAdminDelete
instance ToJSON AdminDelete where
  toJSON = genericToJSON optionsAdminDelete

optionsAdminDelete :: Options
optionsAdminDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data AdminExecute = AdminExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminExecute where
  parseJSON = genericParseJSON optionsAdminExecute
instance ToJSON AdminExecute where
  toJSON = genericToJSON optionsAdminExecute

optionsAdminExecute :: Options
optionsAdminExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype AdminExportFolderCreateVM = AdminExportFolderCreateVM { unAdminExportFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminFolderCreateVM = AdminFolderCreateVM { unAdminFolderCreateVM :: FolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data AdminGet = AdminGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminGet where
  parseJSON = genericParseJSON optionsAdminGet
instance ToJSON AdminGet where
  toJSON = genericToJSON optionsAdminGet

optionsAdminGet :: Options
optionsAdminGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data AdminPermission = AdminPermission
  { adminPermissionCreate :: Maybe AdminCreate -- ^ 
  , adminPermissionDelete :: Maybe AdminDelete -- ^ 
  , adminPermissionExecute :: Maybe AdminExecute -- ^ 
  , adminPermissionGet :: Maybe AdminGet -- ^ 
  , adminPermissionUpdate :: Maybe AdminUpdate -- ^ 
  , adminPermissionAdministrate :: Maybe AdminAdministrate -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminPermission where
  parseJSON = genericParseJSON optionsAdminPermission
instance ToJSON AdminPermission where
  toJSON = genericToJSON optionsAdminPermission

optionsAdminPermission :: Options
optionsAdminPermission =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("adminPermissionCreate", "create")
      , ("adminPermissionDelete", "delete")
      , ("adminPermissionExecute", "execute")
      , ("adminPermissionGet", "get")
      , ("adminPermissionUpdate", "update")
      , ("adminPermissionAdministrate", "administrate")
      ]


-- | 
newtype AdminReportFolderCreateVM = AdminReportFolderCreateVM { unAdminReportFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminSubscriptionVM = AdminSubscriptionVM { unAdminSubscriptionVM :: SubscriptionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminSubscriptionsVM = AdminSubscriptionsVM { unAdminSubscriptionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AdminTemplateFolderCreateVM = AdminTemplateFolderCreateVM { unAdminTemplateFolderCreateVM :: AdminFolderCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data AdminUpdate = AdminUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminUpdate where
  parseJSON = genericParseJSON optionsAdminUpdate
instance ToJSON AdminUpdate where
  toJSON = genericToJSON optionsAdminUpdate

optionsAdminUpdate :: Options
optionsAdminUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype AdminUpdateCurrentSubscriptionPlanVM = AdminUpdateCurrentSubscriptionPlanVM { unAdminUpdateCurrentSubscriptionPlanVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AnalysisResultVM = AnalysisResultVM { unAnalysisResultVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AnalysisResultsVM = AnalysisResultsVM { unAnalysisResultsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ApiKeyVM = ApiKeyVM { unApiKeyVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ApiKeysVM = ApiKeysVM { unApiKeysVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AppMixinsVM = AppMixinsVM { unAppMixinsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditActionVM = AuditActionVM { unAuditActionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditActionsVM = AuditActionsVM { unAuditActionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditActiveStatsVM = AuditActiveStatsVM { unAuditActiveStatsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditFilePropertyChangedVM = AuditFilePropertyChangedVM { unAuditFilePropertyChangedVM :: AuditActionVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditStatVM = AuditStatVM { unAuditStatVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype AuditStatsVM = AuditStatsVM { unAuditStatsVM :: CloudBaseVM }
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
  parseJSON = genericParseJSON optionsAuditType
instance ToJSON AuditType where
  toJSON = genericToJSON optionsAuditType

optionsAuditType :: Options
optionsAuditType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype AuthConfigVM = AuthConfigVM { unAuthConfigVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data BreadcrumbsModel = BreadcrumbsModel
  { breadcrumbsModelId :: Maybe Text -- ^ 
  , breadcrumbsModelName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON BreadcrumbsModel where
  parseJSON = genericParseJSON optionsBreadcrumbsModel
instance ToJSON BreadcrumbsModel where
  toJSON = genericToJSON optionsBreadcrumbsModel

optionsBreadcrumbsModel :: Options
optionsBreadcrumbsModel =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("breadcrumbsModelId", "id")
      , ("breadcrumbsModelName", "name")
      ]


-- | 
newtype BreadcrumbsVM = BreadcrumbsVM { unBreadcrumbsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CheckUserByPasswordInternalVM = CheckUserByPasswordInternalVM { unCheckUserByPasswordInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ClearNotificationsVM = ClearNotificationsVM { unClearNotificationsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data CloudBaseVM = CloudBaseVM
  { cloudBaseVMDollart :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CloudBaseVM where
  parseJSON = genericParseJSON optionsCloudBaseVM
instance ToJSON CloudBaseVM where
  toJSON = genericToJSON optionsCloudBaseVM

optionsCloudBaseVM :: Options
optionsCloudBaseVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("cloudBaseVMDollart", "$t")
      ]


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
  parseJSON = genericParseJSON optionsContactGroupVM
instance ToJSON ContactGroupVM where
  toJSON = genericToJSON optionsContactGroupVM

optionsContactGroupVM :: Options
optionsContactGroupVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contactGroupVMId", "id")
      , ("contactGroupVMName", "name")
      , ("contactGroupVMSubscriptionId", "subscriptionId")
      , ("contactGroupVMCreatedTime", "createdTime")
      , ("contactGroupVMCreatorUserId", "creatorUserId")
      , ("contactGroupVMEditedTime", "editedTime")
      , ("contactGroupVMEditorUserId", "editorUserId")
      ]


-- | 
data ContactGroupsVM = ContactGroupsVM
  { contactGroupsVMGroups :: Maybe [ContactGroupVM] -- ^ 
  , contactGroupsVMSkip :: Maybe Int -- ^ 
  , contactGroupsVMTake :: Maybe Int -- ^ 
  , contactGroupsVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactGroupsVM where
  parseJSON = genericParseJSON optionsContactGroupsVM
instance ToJSON ContactGroupsVM where
  toJSON = genericToJSON optionsContactGroupsVM

optionsContactGroupsVM :: Options
optionsContactGroupsVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contactGroupsVMGroups", "groups")
      , ("contactGroupsVMSkip", "skip")
      , ("contactGroupsVMTake", "take")
      , ("contactGroupsVMCount", "count")
      ]


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
  parseJSON = genericParseJSON optionsContactVM
instance ToJSON ContactVM where
  toJSON = genericToJSON optionsContactVM

optionsContactVM :: Options
optionsContactVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contactVMId", "id")
      , ("contactVMName", "name")
      , ("contactVMEmail", "email")
      , ("contactVMGroups", "groups")
      , ("contactVMSubscriptionId", "subscriptionId")
      , ("contactVMCreatedTime", "createdTime")
      , ("contactVMCreatorUserId", "creatorUserId")
      , ("contactVMEditedTime", "editedTime")
      , ("contactVMEditorUserId", "editorUserId")
      ]


-- | 
data ContactsVM = ContactsVM
  { contactsVMContacts :: Maybe [ContactVM] -- ^ 
  , contactsVMSkip :: Maybe Int -- ^ 
  , contactsVMTake :: Maybe Int -- ^ 
  , contactsVMCount :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ContactsVM where
  parseJSON = genericParseJSON optionsContactsVM
instance ToJSON ContactsVM where
  toJSON = genericToJSON optionsContactsVM

optionsContactsVM :: Options
optionsContactsVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("contactsVMContacts", "contacts")
      , ("contactsVMSkip", "skip")
      , ("contactsVMTake", "take")
      , ("contactsVMCount", "count")
      ]


-- | 
newtype CountVM = CountVM { unCountVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateApiKeyVM = CreateApiKeyVM { unCreateApiKeyVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateAuditActionVM = CreateAuditActionVM { unCreateAuditActionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data CreateContactGroupVM = CreateContactGroupVM
  { createContactGroupVMName :: Text -- ^ 
  , createContactGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactGroupVM where
  parseJSON = genericParseJSON optionsCreateContactGroupVM
instance ToJSON CreateContactGroupVM where
  toJSON = genericToJSON optionsCreateContactGroupVM

optionsCreateContactGroupVM :: Options
optionsCreateContactGroupVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("createContactGroupVMName", "name")
      , ("createContactGroupVMSubscriptionId", "subscriptionId")
      ]


-- | 
data CreateContactVM = CreateContactVM
  { createContactVMName :: Maybe Text -- ^ 
  , createContactVMEmail :: Maybe Text -- ^ 
  , createContactVMGroups :: Maybe [Text] -- ^ 
  , createContactVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateContactVM where
  parseJSON = genericParseJSON optionsCreateContactVM
instance ToJSON CreateContactVM where
  toJSON = genericToJSON optionsCreateContactVM

optionsCreateContactVM :: Options
optionsCreateContactVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("createContactVMName", "name")
      , ("createContactVMEmail", "email")
      , ("createContactVMGroups", "groups")
      , ("createContactVMSubscriptionId", "subscriptionId")
      ]


-- | 
newtype CreateDataSourceAdminVM = CreateDataSourceAdminVM { unCreateDataSourceAdminVM :: CreateDataSourceVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateDataSourceVM = CreateDataSourceVM { unCreateDataSourceVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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
data CreateFileShareVM = CreateFileShareVM
  { createFileShareVMExpires :: Maybe UTCTime -- ^ 
  , createFileShareVMName :: Maybe Text -- ^ 
  , createFileShareVMPermission :: Maybe FilePermissionCRUDVM -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateFileShareVM where
  parseJSON = genericParseJSON optionsCreateFileShareVM
instance ToJSON CreateFileShareVM where
  toJSON = genericToJSON optionsCreateFileShareVM

optionsCreateFileShareVM :: Options
optionsCreateFileShareVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("createFileShareVMExpires", "expires")
      , ("createFileShareVMName", "name")
      , ("createFileShareVMPermission", "permission")
      ]


-- | 
newtype CreateGroupAdminVM = CreateGroupAdminVM { unCreateGroupAdminVM :: CreateGroupVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateGroupVM = CreateGroupVM { unCreateGroupVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateIfNotExistInternalVM = CreateIfNotExistInternalVM { unCreateIfNotExistInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreatePrepareTemplateTaskVM = CreatePrepareTemplateTaskVM { unCreatePrepareTemplateTaskVM :: CreateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateSubscriptionInviteVM = CreateSubscriptionInviteVM { unCreateSubscriptionInviteVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateSubscriptionPeriodVM = CreateSubscriptionPeriodVM { unCreateSubscriptionPeriodVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateSubscriptionPlanVM = CreateSubscriptionPlanVM { unCreateSubscriptionPlanVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateSubscriptionVM = CreateSubscriptionVM { unCreateSubscriptionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype CreateTaskBaseVM = CreateTaskBaseVM { unCreateTaskBaseVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data CreateTaskEndVM = CreateTaskEndVM
  { createTaskEndVMAfter :: Maybe Int -- ^ 
  , createTaskEndVMOn :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateTaskEndVM where
  parseJSON = genericParseJSON optionsCreateTaskEndVM
instance ToJSON CreateTaskEndVM where
  toJSON = genericToJSON optionsCreateTaskEndVM

optionsCreateTaskEndVM :: Options
optionsCreateTaskEndVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("createTaskEndVMAfter", "after")
      , ("createTaskEndVMOn", "on")
      ]


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
  parseJSON = genericParseJSON optionsDataSourceAdministrate
instance ToJSON DataSourceAdministrate where
  toJSON = genericToJSON optionsDataSourceAdministrate

optionsDataSourceAdministrate :: Options
optionsDataSourceAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceConnectionType = DataSourceConnectionType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceConnectionType where
  parseJSON = genericParseJSON optionsDataSourceConnectionType
instance ToJSON DataSourceConnectionType where
  toJSON = genericToJSON optionsDataSourceConnectionType

optionsDataSourceConnectionType :: Options
optionsDataSourceConnectionType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceCreate = DataSourceCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceCreate where
  parseJSON = genericParseJSON optionsDataSourceCreate
instance ToJSON DataSourceCreate where
  toJSON = genericToJSON optionsDataSourceCreate

optionsDataSourceCreate :: Options
optionsDataSourceCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceDelete = DataSourceDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceDelete where
  parseJSON = genericParseJSON optionsDataSourceDelete
instance ToJSON DataSourceDelete where
  toJSON = genericToJSON optionsDataSourceDelete

optionsDataSourceDelete :: Options
optionsDataSourceDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceExecute = DataSourceExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceExecute where
  parseJSON = genericParseJSON optionsDataSourceExecute
instance ToJSON DataSourceExecute where
  toJSON = genericToJSON optionsDataSourceExecute

optionsDataSourceExecute :: Options
optionsDataSourceExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceGet = DataSourceGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceGet where
  parseJSON = genericParseJSON optionsDataSourceGet
instance ToJSON DataSourceGet where
  toJSON = genericToJSON optionsDataSourceGet

optionsDataSourceGet :: Options
optionsDataSourceGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceParameterTypeVM = DataSourceParameterTypeVM
  { dataSourceParameterTypeVMName :: Maybe Text -- ^ 
  , dataSourceParameterTypeVMValue :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceParameterTypeVM where
  parseJSON = genericParseJSON optionsDataSourceParameterTypeVM
instance ToJSON DataSourceParameterTypeVM where
  toJSON = genericToJSON optionsDataSourceParameterTypeVM

optionsDataSourceParameterTypeVM :: Options
optionsDataSourceParameterTypeVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("dataSourceParameterTypeVMName", "name")
      , ("dataSourceParameterTypeVMValue", "value")
      ]


-- | 
newtype DataSourceParameterTypesVM = DataSourceParameterTypesVM { unDataSourceParameterTypesVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourcePermissionCRUDVM = DataSourcePermissionCRUDVM { unDataSourcePermissionCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourcePermissionsCRUDVM = DataSourcePermissionsCRUDVM { unDataSourcePermissionsCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourcePermissionsVM = DataSourcePermissionsVM { unDataSourcePermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourceSelectCommandParameterVM = DataSourceSelectCommandParameterVM { unDataSourceSelectCommandParameterVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourceSelectCommandVM = DataSourceSelectCommandVM { unDataSourceSelectCommandVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data DataSourceSorting = DataSourceSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceSorting where
  parseJSON = genericParseJSON optionsDataSourceSorting
instance ToJSON DataSourceSorting where
  toJSON = genericToJSON optionsDataSourceSorting

optionsDataSourceSorting :: Options
optionsDataSourceSorting =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceStatus = DataSourceStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceStatus where
  parseJSON = genericParseJSON optionsDataSourceStatus
instance ToJSON DataSourceStatus where
  toJSON = genericToJSON optionsDataSourceStatus

optionsDataSourceStatus :: Options
optionsDataSourceStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data DataSourceUpdate = DataSourceUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourceUpdate where
  parseJSON = genericParseJSON optionsDataSourceUpdate
instance ToJSON DataSourceUpdate where
  toJSON = genericToJSON optionsDataSourceUpdate

optionsDataSourceUpdate :: Options
optionsDataSourceUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype DataSourceVM = DataSourceVM { unDataSourceVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DataSourcesVM = DataSourcesVM { unDataSourcesVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DefaultPermissionsVM = DefaultPermissionsVM { unDefaultPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype DeleteApiKeyVM = DeleteApiKeyVM { unDeleteApiKeyVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype EmailTaskVM = EmailTaskVM { unEmailTaskVM :: TransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data EntityType = EntityType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON EntityType where
  parseJSON = genericParseJSON optionsEntityType
instance ToJSON EntityType where
  toJSON = genericToJSON optionsEntityType

optionsEntityType :: Options
optionsEntityType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


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
  parseJSON = genericParseJSON optionsExportFormat
instance ToJSON ExportFormat where
  toJSON = genericToJSON optionsExportFormat

optionsExportFormat :: Options
optionsExportFormat =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype ExportReportTaskVM = ExportReportTaskVM { unExportReportTaskVM :: TransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportReportVM = ExportReportVM { unExportReportVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportTemplateTaskVM = ExportTemplateTaskVM { unExportTemplateTaskVM :: ExportReportTaskVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportTemplateVM = ExportTemplateVM { unExportTemplateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportVM = ExportVM { unExportVM :: FileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ExportsVM = ExportsVM { unExportsVM :: FilesVMBase }
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
  parseJSON = genericParseJSON optionsFileAdministrate
instance ToJSON FileAdministrate where
  toJSON = genericToJSON optionsFileAdministrate

optionsFileAdministrate :: Options
optionsFileAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FileContentVM = FileContentVM { unFileContentVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileCreate = FileCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileCreate where
  parseJSON = genericParseJSON optionsFileCreate
instance ToJSON FileCreate where
  toJSON = genericToJSON optionsFileCreate

optionsFileCreate :: Options
optionsFileCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FileCreateFormVM = FileCreateFormVM { unFileCreateFormVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileCreateVM = FileCreateVM { unFileCreateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileDelete = FileDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileDelete where
  parseJSON = genericParseJSON optionsFileDelete
instance ToJSON FileDelete where
  toJSON = genericToJSON optionsFileDelete

optionsFileDelete :: Options
optionsFileDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FileExecute = FileExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileExecute where
  parseJSON = genericParseJSON optionsFileExecute
instance ToJSON FileExecute where
  toJSON = genericToJSON optionsFileExecute

optionsFileExecute :: Options
optionsFileExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FileGet = FileGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileGet where
  parseJSON = genericParseJSON optionsFileGet
instance ToJSON FileGet where
  toJSON = genericToJSON optionsFileGet

optionsFileGet :: Options
optionsFileGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FileIconVM = FileIconVM { unFileIconVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileKind = FileKind
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileKind where
  parseJSON = genericParseJSON optionsFileKind
instance ToJSON FileKind where
  toJSON = genericToJSON optionsFileKind

optionsFileKind :: Options
optionsFileKind =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FilePermissionCRUDVM = FilePermissionCRUDVM { unFilePermissionCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FilePermissionsCRUDVM = FilePermissionsCRUDVM { unFilePermissionsCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FilePermissionsVM = FilePermissionsVM { unFilePermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileRenameVM = FileRenameVM { unFileRenameVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileShareVM = FileShareVM
  { fileShareVMExpires :: Maybe UTCTime -- ^ 
  , fileShareVMName :: Maybe Text -- ^ 
  , fileShareVMPermission :: Maybe FilePermissionCRUDVM -- ^ 
  , fileShareVMKey :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileShareVM where
  parseJSON = genericParseJSON optionsFileShareVM
instance ToJSON FileShareVM where
  toJSON = genericToJSON optionsFileShareVM

optionsFileShareVM :: Options
optionsFileShareVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fileShareVMExpires", "expires")
      , ("fileShareVMName", "name")
      , ("fileShareVMPermission", "permission")
      , ("fileShareVMKey", "key")
      ]


-- | 
data FileSharingKeysVM = FileSharingKeysVM
  { fileSharingKeysVMKeys :: Maybe [FileShareVM] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileSharingKeysVM where
  parseJSON = genericParseJSON optionsFileSharingKeysVM
instance ToJSON FileSharingKeysVM where
  toJSON = genericToJSON optionsFileSharingKeysVM

optionsFileSharingKeysVM :: Options
optionsFileSharingKeysVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("fileSharingKeysVMKeys", "keys")
      ]


-- | 
data FileSorting = FileSorting
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileSorting where
  parseJSON = genericParseJSON optionsFileSorting
instance ToJSON FileSorting where
  toJSON = genericToJSON optionsFileSorting

optionsFileSorting :: Options
optionsFileSorting =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FileStatus = FileStatus
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatus where
  parseJSON = genericParseJSON optionsFileStatus
instance ToJSON FileStatus where
  toJSON = genericToJSON optionsFileStatus

optionsFileStatus :: Options
optionsFileStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FileStatusReason = FileStatusReason
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileStatusReason where
  parseJSON = genericParseJSON optionsFileStatusReason
instance ToJSON FileStatusReason where
  toJSON = genericToJSON optionsFileStatusReason

optionsFileStatusReason :: Options
optionsFileStatusReason =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FileStatusUpdateInternalVM = FileStatusUpdateInternalVM { unFileStatusUpdateInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileStatusVM = FileStatusVM { unFileStatusVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileTagsUpdateVM = FileTagsUpdateVM { unFileTagsUpdateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileThumbnailUpdateInternalVM = FileThumbnailUpdateInternalVM { unFileThumbnailUpdateInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data FileType = FileType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileType where
  parseJSON = genericParseJSON optionsFileType
instance ToJSON FileType where
  toJSON = genericToJSON optionsFileType

optionsFileType :: Options
optionsFileType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data FileUpdate = FileUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileUpdate where
  parseJSON = genericParseJSON optionsFileUpdate
instance ToJSON FileUpdate where
  toJSON = genericToJSON optionsFileUpdate

optionsFileUpdate :: Options
optionsFileUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype FileUpdateVM = FileUpdateVM { unFileUpdateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FileVM = FileVM { unFileVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FilesVM = FilesVM { unFilesVM :: FilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FilesVMBase = FilesVMBase { unFilesVMBase :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FolderCreateVM = FolderCreateVM { unFolderCreateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FolderIconVM = FolderIconVM { unFolderIconVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FolderRenameVM = FolderRenameVM { unFolderRenameVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FolderSizeVM = FolderSizeVM { unFolderSizeVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FolderTagsUpdateVM = FolderTagsUpdateVM { unFolderTagsUpdateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype FrontendAppVM = FrontendAppVM { unFrontendAppVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data GroupAdministrate = GroupAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupAdministrate where
  parseJSON = genericParseJSON optionsGroupAdministrate
instance ToJSON GroupAdministrate where
  toJSON = genericToJSON optionsGroupAdministrate

optionsGroupAdministrate :: Options
optionsGroupAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data GroupCreate = GroupCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupCreate where
  parseJSON = genericParseJSON optionsGroupCreate
instance ToJSON GroupCreate where
  toJSON = genericToJSON optionsGroupCreate

optionsGroupCreate :: Options
optionsGroupCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data GroupDelete = GroupDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupDelete where
  parseJSON = genericParseJSON optionsGroupDelete
instance ToJSON GroupDelete where
  toJSON = genericToJSON optionsGroupDelete

optionsGroupDelete :: Options
optionsGroupDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data GroupExecute = GroupExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupExecute where
  parseJSON = genericParseJSON optionsGroupExecute
instance ToJSON GroupExecute where
  toJSON = genericToJSON optionsGroupExecute

optionsGroupExecute :: Options
optionsGroupExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data GroupGet = GroupGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupGet where
  parseJSON = genericParseJSON optionsGroupGet
instance ToJSON GroupGet where
  toJSON = genericToJSON optionsGroupGet

optionsGroupGet :: Options
optionsGroupGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype GroupPermissionCRUDVM = GroupPermissionCRUDVM { unGroupPermissionCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype GroupPermissionsCRUDVM = GroupPermissionsCRUDVM { unGroupPermissionsCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype GroupPermissionsVM = GroupPermissionsVM { unGroupPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data GroupUpdate = GroupUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupUpdate where
  parseJSON = genericParseJSON optionsGroupUpdate
instance ToJSON GroupUpdate where
  toJSON = genericToJSON optionsGroupUpdate

optionsGroupUpdate :: Options
optionsGroupUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype GroupUserVM = GroupUserVM { unGroupUserVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype GroupUsersVM = GroupUsersVM { unGroupUsersVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype GroupVM = GroupVM { unGroupVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype GroupsVM = GroupsVM { unGroupsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype HttpValidationProblemDetails = HttpValidationProblemDetails { unHttpValidationProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype InputFileVM = InputFileVM { unInputFileVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data InvitedUser = InvitedUser
  { invitedUserUserId :: Maybe Text -- ^ 
  , invitedUserInvitedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InvitedUser where
  parseJSON = genericParseJSON optionsInvitedUser
instance ToJSON InvitedUser where
  toJSON = genericToJSON optionsInvitedUser

optionsInvitedUser :: Options
optionsInvitedUser =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("invitedUserUserId", "userId")
      , ("invitedUserInvitedAt", "invitedAt")
      ]


-- | 
newtype MyPermissionsVM = MyPermissionsVM { unMyPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype OutputFileVM = OutputFileVM { unOutputFileVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype PrepareTemplateTaskVM = PrepareTemplateTaskVM { unPrepareTemplateTaskVM :: TransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype PrepareTemplateVM = PrepareTemplateVM { unPrepareTemplateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype PreviewReportVM = PreviewReportVM { unPreviewReportVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype PreviewTemplateVM = PreviewTemplateVM { unPreviewTemplateVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ProblemDetails = ProblemDetails { unProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data ProblemLevel = ProblemLevel
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemLevel where
  parseJSON = genericParseJSON optionsProblemLevel
instance ToJSON ProblemLevel where
  toJSON = genericToJSON optionsProblemLevel

optionsProblemLevel :: Options
optionsProblemLevel =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data ProblemType = ProblemType
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProblemType where
  parseJSON = genericParseJSON optionsProblemType
instance ToJSON ProblemType where
  toJSON = genericToJSON optionsProblemType

optionsProblemType :: Options
optionsProblemType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data ProfileVisibility = ProfileVisibility
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ProfileVisibility where
  parseJSON = genericParseJSON optionsProfileVisibility
instance ToJSON ProfileVisibility where
  toJSON = genericToJSON optionsProfileVisibility

optionsProfileVisibility :: Options
optionsProfileVisibility =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype RegisterUserVM = RegisterUserVM { unRegisterUserVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RenameDataSourceVM = RenameDataSourceVM { unRenameDataSourceVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RenameGroupVM = RenameGroupVM { unRenameGroupVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RenameSubscriptionVM = RenameSubscriptionVM { unRenameSubscriptionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ReportCreateAdminVM = ReportCreateAdminVM { unReportCreateAdminVM :: ReportCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ReportCreateFormVM = ReportCreateFormVM { unReportCreateFormVM :: FileCreateFormVM }
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
  parseJSON = genericParseJSON optionsReportInfo
instance ToJSON ReportInfo where
  toJSON = genericToJSON optionsReportInfo

optionsReportInfo :: Options
optionsReportInfo =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("reportInfoAuthor", "author")
      , ("reportInfoCreated", "created")
      , ("reportInfoCreatorVersion", "creatorVersion")
      , ("reportInfoDescription", "description")
      , ("reportInfoModified", "modified")
      , ("reportInfoName", "name")
      , ("reportInfoPicture", "picture")
      , ("reportInfoPreviewPictureRatio", "previewPictureRatio")
      , ("reportInfoSaveMode", "saveMode")
      , ("reportInfoSavePreviewPicture", "savePreviewPicture")
      , ("reportInfoTag", "tag")
      , ("reportInfoVersion", "version")
      ]


-- | 
newtype ReportVM = ReportVM { unReportVM :: FileVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ReportsVM = ReportsVM { unReportsVM :: FilesVMBase }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype RestOfSpaceVM = RestOfSpaceVM { unRestOfSpaceVM :: CloudBaseVM }
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
newtype RunTaskBaseVM = RunTaskBaseVM { unRunTaskBaseVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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
  parseJSON = genericParseJSON optionsSaveMode
instance ToJSON SaveMode where
  toJSON = genericToJSON optionsSaveMode

optionsSaveMode :: Options
optionsSaveMode =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype SelectedFilesVM = SelectedFilesVM { unSelectedFilesVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype ServerConfigurationVM = ServerConfigurationVM { unServerConfigurationVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SolvationReportVM = SolvationReportVM { unSolvationReportVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data SubscriptionAdministrate = SubscriptionAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionAdministrate where
  parseJSON = genericParseJSON optionsSubscriptionAdministrate
instance ToJSON SubscriptionAdministrate where
  toJSON = genericToJSON optionsSubscriptionAdministrate

optionsSubscriptionAdministrate :: Options
optionsSubscriptionAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SubscriptionCreate = SubscriptionCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionCreate where
  parseJSON = genericParseJSON optionsSubscriptionCreate
instance ToJSON SubscriptionCreate where
  toJSON = genericToJSON optionsSubscriptionCreate

optionsSubscriptionCreate :: Options
optionsSubscriptionCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SubscriptionDelete = SubscriptionDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionDelete where
  parseJSON = genericParseJSON optionsSubscriptionDelete
instance ToJSON SubscriptionDelete where
  toJSON = genericToJSON optionsSubscriptionDelete

optionsSubscriptionDelete :: Options
optionsSubscriptionDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SubscriptionExecute = SubscriptionExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionExecute where
  parseJSON = genericParseJSON optionsSubscriptionExecute
instance ToJSON SubscriptionExecute where
  toJSON = genericToJSON optionsSubscriptionExecute

optionsSubscriptionExecute :: Options
optionsSubscriptionExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data SubscriptionFolder = SubscriptionFolder
  { subscriptionFolderFolderId :: Maybe Text -- ^ 
  , subscriptionFolderBytesUsed :: Maybe Integer -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionFolder where
  parseJSON = genericParseJSON optionsSubscriptionFolder
instance ToJSON SubscriptionFolder where
  toJSON = genericToJSON optionsSubscriptionFolder

optionsSubscriptionFolder :: Options
optionsSubscriptionFolder =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("subscriptionFolderFolderId", "folderId")
      , ("subscriptionFolderBytesUsed", "bytesUsed")
      ]


-- | 
data SubscriptionGet = SubscriptionGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionGet where
  parseJSON = genericParseJSON optionsSubscriptionGet
instance ToJSON SubscriptionGet where
  toJSON = genericToJSON optionsSubscriptionGet

optionsSubscriptionGet :: Options
optionsSubscriptionGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype SubscriptionInviteVM = SubscriptionInviteVM { unSubscriptionInviteVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionInvitesVM = SubscriptionInvitesVM { unSubscriptionInvitesVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPeriodVM = SubscriptionPeriodVM { unSubscriptionPeriodVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPermissionCRUDVM = SubscriptionPermissionCRUDVM { unSubscriptionPermissionCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPermissionsCRUDVM = SubscriptionPermissionsCRUDVM { unSubscriptionPermissionsCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPermissionsVM = SubscriptionPermissionsVM { unSubscriptionPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPlanVM = SubscriptionPlanVM { unSubscriptionPlanVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionPlansVM = SubscriptionPlansVM { unSubscriptionPlansVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data SubscriptionUpdate = SubscriptionUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON SubscriptionUpdate where
  parseJSON = genericParseJSON optionsSubscriptionUpdate
instance ToJSON SubscriptionUpdate where
  toJSON = genericToJSON optionsSubscriptionUpdate

optionsSubscriptionUpdate :: Options
optionsSubscriptionUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype SubscriptionUserVM = SubscriptionUserVM { unSubscriptionUserVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionUsersVM = SubscriptionUsersVM { unSubscriptionUsersVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionVM = SubscriptionVM { unSubscriptionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype SubscriptionsVM = SubscriptionsVM { unSubscriptionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TaskAdministrate = TaskAdministrate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskAdministrate where
  parseJSON = genericParseJSON optionsTaskAdministrate
instance ToJSON TaskAdministrate where
  toJSON = genericToJSON optionsTaskAdministrate

optionsTaskAdministrate :: Options
optionsTaskAdministrate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype TaskBaseVM = TaskBaseVM { unTaskBaseVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TaskCreate = TaskCreate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskCreate where
  parseJSON = genericParseJSON optionsTaskCreate
instance ToJSON TaskCreate where
  toJSON = genericToJSON optionsTaskCreate

optionsTaskCreate :: Options
optionsTaskCreate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data TaskDelete = TaskDelete
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskDelete where
  parseJSON = genericParseJSON optionsTaskDelete
instance ToJSON TaskDelete where
  toJSON = genericToJSON optionsTaskDelete

optionsTaskDelete :: Options
optionsTaskDelete =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data TaskEnd = TaskEnd
  { taskEndAfter :: Maybe Int -- ^ 
  , taskEndOn :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskEnd where
  parseJSON = genericParseJSON optionsTaskEnd
instance ToJSON TaskEnd where
  toJSON = genericToJSON optionsTaskEnd

optionsTaskEnd :: Options
optionsTaskEnd =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("taskEndAfter", "after")
      , ("taskEndOn", "on")
      ]


-- | 
data TaskExecute = TaskExecute
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskExecute where
  parseJSON = genericParseJSON optionsTaskExecute
instance ToJSON TaskExecute where
  toJSON = genericToJSON optionsTaskExecute

optionsTaskExecute :: Options
optionsTaskExecute =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data TaskGet = TaskGet
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskGet where
  parseJSON = genericParseJSON optionsTaskGet
instance ToJSON TaskGet where
  toJSON = genericToJSON optionsTaskGet

optionsTaskGet :: Options
optionsTaskGet =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype TaskIdsVM = TaskIdsVM { unTaskIdsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TaskMessageIdVM = TaskMessageIdVM { unTaskMessageIdVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TaskPermissionCRUDVM = TaskPermissionCRUDVM { unTaskPermissionCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TaskPermissionsCRUDVM = TaskPermissionsCRUDVM { unTaskPermissionsCRUDVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TaskPermissionsVM = TaskPermissionsVM { unTaskPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TaskSettingsVM = TaskSettingsVM { unTaskSettingsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data TaskUpdate = TaskUpdate
  { 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TaskUpdate where
  parseJSON = genericParseJSON optionsTaskUpdate
instance ToJSON TaskUpdate where
  toJSON = genericToJSON optionsTaskUpdate

optionsTaskUpdate :: Options
optionsTaskUpdate =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
newtype TasksVM = TasksVM { unTasksVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateContentVM = TemplateContentVM { unTemplateContentVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateCreateAdminVM = TemplateCreateAdminVM { unTemplateCreateAdminVM :: TemplateCreateVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype TemplateCreateFormVM = TemplateCreateFormVM { unTemplateCreateFormVM :: FileCreateFormVM }
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
newtype TemplatesVM = TemplatesVM { unTemplatesVM :: FilesVMBase }
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
  parseJSON = genericParseJSON optionsTimePeriodType
instance ToJSON TimePeriodType where
  toJSON = genericToJSON optionsTimePeriodType

optionsTimePeriodType :: Options
optionsTimePeriodType =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ 
      ]


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
  parseJSON = genericParseJSON optionsUpdateContactGroupVM
instance ToJSON UpdateContactGroupVM where
  toJSON = genericToJSON optionsUpdateContactGroupVM

optionsUpdateContactGroupVM :: Options
optionsUpdateContactGroupVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("updateContactGroupVMName", "name")
      , ("updateContactGroupVMSubscriptionId", "subscriptionId")
      ]


-- | 
data UpdateContactVM = UpdateContactVM
  { updateContactVMName :: Maybe Text -- ^ 
  , updateContactVMEmail :: Maybe Text -- ^ 
  , updateContactVMGroups :: Maybe [Text] -- ^ 
  , updateContactVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateContactVM where
  parseJSON = genericParseJSON optionsUpdateContactVM
instance ToJSON UpdateContactVM where
  toJSON = genericToJSON optionsUpdateContactVM

optionsUpdateContactVM :: Options
optionsUpdateContactVM =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ lookup s table
    }
  where
    table =
      [ ("updateContactVMName", "name")
      , ("updateContactVMEmail", "email")
      , ("updateContactVMGroups", "groups")
      , ("updateContactVMSubscriptionId", "subscriptionId")
      ]


-- | 
newtype UpdateContentInternalVM = UpdateContentInternalVM { unUpdateContentInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataSourceConnectionStringVM = UpdateDataSourceConnectionStringVM { unUpdateDataSourceConnectionStringVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataSourcePermissionsVM = UpdateDataSourcePermissionsVM { unUpdateDataSourcePermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataSourceSelectCommandsVM = UpdateDataSourceSelectCommandsVM { unUpdateDataSourceSelectCommandsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataSourceSubscriptionVM = UpdateDataSourceSubscriptionVM { unUpdateDataSourceSubscriptionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataSourceVM = UpdateDataSourceVM { unUpdateDataSourceVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDataVM = UpdateDataVM { unUpdateDataVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateDefaultPermissionsVM = UpdateDefaultPermissionsVM { unUpdateDefaultPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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
newtype UpdateFileContentFormVM = UpdateFileContentFormVM { unUpdateFileContentFormVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateFileContentInternalVM = UpdateFileContentInternalVM { unUpdateFileContentInternalVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateFileContentVM = UpdateFileContentVM { unUpdateFileContentVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateFilePermissionsVM = UpdateFilePermissionsVM { unUpdateFilePermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateGroupPermissionsVM = UpdateGroupPermissionsVM { unUpdateGroupPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateGroupVM = UpdateGroupVM { unUpdateGroupVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdatePrepareTemplateTaskVM = UpdatePrepareTemplateTaskVM { unUpdatePrepareTemplateTaskVM :: UpdateTransformTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateSubscriptionLocaleVM = UpdateSubscriptionLocaleVM { unUpdateSubscriptionLocaleVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateSubscriptionPermissionsVM = UpdateSubscriptionPermissionsVM { unUpdateSubscriptionPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateSubscriptionPlanVM = UpdateSubscriptionPlanVM { unUpdateSubscriptionPlanVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateSubscriptionVM = UpdateSubscriptionVM { unUpdateSubscriptionVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateTaskBaseVM = UpdateTaskBaseVM { unUpdateTaskBaseVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateTaskPermissionsVM = UpdateTaskPermissionsVM { unUpdateTaskPermissionsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

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
newtype UpdateUserProfileVM = UpdateUserProfileVM { unUpdateUserProfileVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateUserSettingsVM = UpdateUserSettingsVM { unUpdateUserSettingsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateUserVM = UpdateUserVM { unUpdateUserVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UpdateWebhookTaskVM = UpdateWebhookTaskVM { unUpdateWebhookTaskVM :: UpdateTransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UserIsAdminVM = UserIsAdminVM { unUserIsAdminVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UserProfileVM = UserProfileVM { unUserProfileVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UserResultVM = UserResultVM { unUserResultVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UserSettingsVM = UserSettingsVM { unUserSettingsVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UserVM = UserVM { unUserVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype UsersVM = UsersVM { unUsersVM :: CloudBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
newtype WebhookTaskVM = WebhookTaskVM { unWebhookTaskVM :: TransportTaskBaseVM }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)
