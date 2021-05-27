{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module FastReportCloud.Types (
  AdminExportFolderCreateVM (..),
  AdminPermission (..),
  AdminReportFolderCreateVM (..),
  AdminSubscriptionVM (..),
  AdminSubscriptionsVM (..),
  AdminTemplateFolderCreateVM (..),
  AnalysisResultVM (..),
  AnalysisResultsVM (..),
  ApiKeyVM (..),
  ApiKeysVM (..),
  BreadcrumbsModel (..),
  BreadcrumbsVM (..),
  CountVM (..),
  CreateApiKeyVM (..),
  CreateDataSourceAdminVM (..),
  CreateDataSourceVM (..),
  CreateGroupAdminVM (..),
  CreateGroupVM (..),
  CreateSubscriptionInviteVM (..),
  CreateSubscriptionPeriodVM (..),
  CreateSubscriptionPlanVM (..),
  CreateSubscriptionVM (..),
  DataSourcePermission (..),
  DataSourcePermissions (..),
  DataSourcePermissionsVM (..),
  DataSourceVM (..),
  DataSourcesVM (..),
  DefaultPermissions (..),
  DefaultPermissionsVM (..),
  DeleteApiKeyVM (..),
  ExportCreateAdminVM (..),
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
  FileUpdateVM (..),
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
  RegisterUserVM (..),
  RenameDataSourceVM (..),
  RenameGroupVM (..),
  RenameSubscriptionVM (..),
  ReportCreateAdminVM (..),
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
  TemplateCreateAdminVM (..),
  TemplateCreateVM (..),
  TemplateFolderCreateVM (..),
  TemplateVM (..),
  TemplatesVM (..),
  UpdateDataSourceConnectionStringVM (..),
  UpdateDataSourcePermissionsVM (..),
  UpdateDataSourceSubscriptionVM (..),
  UpdateDataSourceVM (..),
  UpdateDefaultPermissionsVM (..),
  UpdateFilePermissionsVM (..),
  UpdateGroupPermissionsVM (..),
  UpdateGroupVM (..),
  UpdateSubscriptionLocaleVM (..),
  UpdateSubscriptionPermissionsVM (..),
  UpdateSubscriptionPlanVM (..),
  UpdateSubscriptionVM (..),
  UpdateUserVM (..),
  UserProfileUpdateVM (..),
  UserProfileVM (..),
  UserSettings (..),
  UserVM (..),
  UsersVM (..),
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
  { adminExportFolderCreateVMParentId :: Maybe Text -- ^ 
  , adminExportFolderCreateVMOwnerId :: Maybe Text -- ^ 
  , adminExportFolderCreateVMName :: Maybe Text -- ^ 
  , adminExportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminExportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminExportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminExportFolderCreateVM")
instance ToJSON AdminExportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminExportFolderCreateVM")


-- | 
data AdminPermission = AdminPermission
  { adminPermissionCreate :: Maybe Int -- ^ 
  , adminPermissionDelete :: Maybe Int -- ^ 
  , adminPermissionExecute :: Maybe Int -- ^ 
  , adminPermissionGet :: Maybe Int -- ^ 
  , adminPermissionUpdate :: Maybe Int -- ^ 
  , adminPermissionAdministrate :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminPermission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminPermission")
instance ToJSON AdminPermission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminPermission")


-- | 
data AdminReportFolderCreateVM = AdminReportFolderCreateVM
  { adminReportFolderCreateVMParentId :: Maybe Text -- ^ 
  , adminReportFolderCreateVMOwnerId :: Maybe Text -- ^ 
  , adminReportFolderCreateVMName :: Maybe Text -- ^ 
  , adminReportFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminReportFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminReportFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminReportFolderCreateVM")
instance ToJSON AdminReportFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminReportFolderCreateVM")


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


-- | 
data AdminSubscriptionsVM = AdminSubscriptionsVM
  { adminSubscriptionsVMSubscriptions :: Maybe [AdminSubscriptionVM] -- ^ 
  , adminSubscriptionsVMCount :: Maybe Integer -- ^ 
  , adminSubscriptionsVMSkip :: Maybe Int -- ^ 
  , adminSubscriptionsVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminSubscriptionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminSubscriptionsVM")
instance ToJSON AdminSubscriptionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminSubscriptionsVM")


-- | 
data AdminTemplateFolderCreateVM = AdminTemplateFolderCreateVM
  { adminTemplateFolderCreateVMParentId :: Maybe Text -- ^ 
  , adminTemplateFolderCreateVMOwnerId :: Maybe Text -- ^ 
  , adminTemplateFolderCreateVMName :: Maybe Text -- ^ 
  , adminTemplateFolderCreateVMTags :: Maybe [Text] -- ^ 
  , adminTemplateFolderCreateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AdminTemplateFolderCreateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "adminTemplateFolderCreateVM")
instance ToJSON AdminTemplateFolderCreateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "adminTemplateFolderCreateVM")


-- | 
data AnalysisResultVM = AnalysisResultVM
  { analysisResultVMLevel :: Maybe Int -- ^ 
  , analysisResultVMDetail :: Maybe Text -- ^ 
  , analysisResultVMId :: Maybe Text -- ^ 
  , analysisResultVMSubscriptionId :: Maybe Text -- ^ 
  , analysisResultVMFileId :: Maybe Text -- ^ 
  , analysisResultVMCollectionName :: Maybe Text -- ^ 
  , analysisResultVMType :: Maybe Int -- ^ 
  , analysisResultVMSignature :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AnalysisResultVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "analysisResultVM")
instance ToJSON AnalysisResultVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "analysisResultVM")


-- | 
data AnalysisResultsVM = AnalysisResultsVM
  { analysisResultsVMResults :: Maybe [AnalysisResultVM] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON AnalysisResultsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "analysisResultsVM")
instance ToJSON AnalysisResultsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "analysisResultsVM")


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
data CreateDataSourceAdminVM = CreateDataSourceAdminVM
  { createDataSourceAdminVMOwnerId :: Maybe Text -- ^ 
  , createDataSourceAdminVMName :: Maybe Text -- ^ 
  , createDataSourceAdminVMConnectionString :: Text -- ^ 
  , createDataSourceAdminVMSubscriptionId :: Maybe Text -- ^ 
  , createDataSourceAdminVMConnectionType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDataSourceAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDataSourceAdminVM")
instance ToJSON CreateDataSourceAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDataSourceAdminVM")


-- | 
data CreateDataSourceVM = CreateDataSourceVM
  { createDataSourceVMName :: Maybe Text -- ^ 
  , createDataSourceVMConnectionString :: Text -- ^ 
  , createDataSourceVMSubscriptionId :: Maybe Text -- ^ 
  , createDataSourceVMConnectionType :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateDataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createDataSourceVM")
instance ToJSON CreateDataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createDataSourceVM")


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
data CreateSubscriptionPeriodVM = CreateSubscriptionPeriodVM
  { createSubscriptionPeriodVMPlanId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSubscriptionPeriodVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSubscriptionPeriodVM")
instance ToJSON CreateSubscriptionPeriodVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSubscriptionPeriodVM")


-- | 
data CreateSubscriptionPlanVM = CreateSubscriptionPlanVM
  { createSubscriptionPlanVMIsActive :: Maybe Bool -- ^ 
  , createSubscriptionPlanVMDisplayName :: Maybe Text -- ^ 
  , createSubscriptionPlanVMTimePeriodType :: Maybe Text -- ^ 
  , createSubscriptionPlanVMTimePeriod :: Maybe Int -- ^ 
  , createSubscriptionPlanVMTemplatesSpaceLimit :: Maybe Integer -- ^ 
  , createSubscriptionPlanVMReportsSpaceLimit :: Maybe Integer -- ^ 
  , createSubscriptionPlanVMExportsSpaceLimit :: Maybe Integer -- ^ 
  , createSubscriptionPlanVMFileUploadSizeLimit :: Maybe Integer -- ^ 
  , createSubscriptionPlanVMDataSourceLimit :: Maybe Int -- ^ 
  , createSubscriptionPlanVMMaxUsersCount :: Maybe Int -- ^ 
  , createSubscriptionPlanVMHasSpaceOverdraft :: Maybe Bool -- ^ 
  , createSubscriptionPlanVMGroupLimit :: Maybe Int -- ^ 
  , createSubscriptionPlanVMOnlineDesigner :: Maybe Bool -- ^ 
  , createSubscriptionPlanVMIsDemo :: Maybe Bool -- ^ 
  , createSubscriptionPlanVMUrlToBuy :: Maybe Text -- ^ 
  , createSubscriptionPlanVMUnlimitedPage :: Maybe Bool -- ^ 
  , createSubscriptionPlanVMPageLimit :: Maybe Int -- ^ 
  , createSubscriptionPlanVMReadonlyTimeLimitType :: Maybe Text -- ^ 
  , createSubscriptionPlanVMReadonlyTimeLimit :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSubscriptionPlanVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSubscriptionPlanVM")
instance ToJSON CreateSubscriptionPlanVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSubscriptionPlanVM")


-- | 
data CreateSubscriptionVM = CreateSubscriptionVM
  { createSubscriptionVMPlanId :: Maybe Text -- ^ 
  , createSubscriptionVMName :: Maybe Text -- ^ 
  , createSubscriptionVMUserId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON CreateSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "createSubscriptionVM")
instance ToJSON CreateSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "createSubscriptionVM")


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


-- | 
data DataSourcePermissionsVM = DataSourcePermissionsVM
  { dataSourcePermissionsVMPermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataSourcePermissionsVM")
instance ToJSON DataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataSourcePermissionsVM")


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
data DefaultPermissions = DefaultPermissions
  { defaultPermissionsFilePermissions :: Maybe FilePermissions -- ^ 
  , defaultPermissionsGroupPermissions :: Maybe GroupPermissions -- ^ 
  , defaultPermissionsDataSourcePermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DefaultPermissions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "defaultPermissions")
instance ToJSON DefaultPermissions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "defaultPermissions")


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


-- | 
data DeleteApiKeyVM = DeleteApiKeyVM
  { deleteApiKeyVMApiKey :: Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON DeleteApiKeyVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "deleteApiKeyVM")
instance ToJSON DeleteApiKeyVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "deleteApiKeyVM")


-- | 
data ExportCreateAdminVM = ExportCreateAdminVM
  { exportCreateAdminVMOwnerId :: Maybe Text -- ^ 
  , exportCreateAdminVMParentId :: Maybe Text -- ^ 
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


-- | 
data ExportReportTaskVM = ExportReportTaskVM
  { exportReportTaskVMFileName :: Maybe Text -- ^ 
  , exportReportTaskVMFolderId :: Maybe Text -- ^ 
  , exportReportTaskVMLocale :: Maybe Text -- ^ 
  , exportReportTaskVMPagesCount :: Maybe Int -- ^ 
  , exportReportTaskVMFormat :: Maybe Text -- ^ 
  , exportReportTaskVMExportParameters :: Maybe (Map.Map String Value) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportReportTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportReportTaskVM")
instance ToJSON ExportReportTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportReportTaskVM")


-- | 
data ExportTemplateTaskVM = ExportTemplateTaskVM
  { exportTemplateTaskVMFileName :: Maybe Text -- ^ 
  , exportTemplateTaskVMFolderId :: Maybe Text -- ^ 
  , exportTemplateTaskVMLocale :: Maybe Text -- ^ 
  , exportTemplateTaskVMPagesCount :: Maybe Int -- ^ 
  , exportTemplateTaskVMFormat :: Maybe Text -- ^ 
  , exportTemplateTaskVMExportParameters :: Maybe (Map.Map String Value) -- ^ 
  , exportTemplateTaskVMReportParameters :: Maybe (Map.Map String Value) -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ExportTemplateTaskVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "exportTemplateTaskVM")
instance ToJSON ExportTemplateTaskVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "exportTemplateTaskVM")


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


-- | 
data FileIconVM = FileIconVM
  { fileIconVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileIconVM")
instance ToJSON FileIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileIconVM")


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
data FileTagsUpdateVM = FileTagsUpdateVM
  { fileTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileTagsUpdateVM")
instance ToJSON FileTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileTagsUpdateVM")


-- | 
data FileUpdateVM = FileUpdateVM
  { fileUpdateVMName :: Maybe Text -- ^ 
  , fileUpdateVMParentId :: Maybe Text -- ^ 
  , fileUpdateVMTags :: Maybe [Text] -- ^ 
  , fileUpdateVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FileUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "fileUpdateVM")
instance ToJSON FileUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "fileUpdateVM")


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


-- | 
data FolderIconVM = FolderIconVM
  { folderIconVMIcon :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderIconVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderIconVM")
instance ToJSON FolderIconVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderIconVM")


-- | 
data FolderRenameVM = FolderRenameVM
  { folderRenameVMName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderRenameVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderRenameVM")
instance ToJSON FolderRenameVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderRenameVM")


-- | 
data FolderTagsUpdateVM = FolderTagsUpdateVM
  { folderTagsUpdateVMTags :: Maybe [Text] -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON FolderTagsUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "folderTagsUpdateVM")
instance ToJSON FolderTagsUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "folderTagsUpdateVM")


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


-- | 
data GroupPermissionsVM = GroupPermissionsVM
  { groupPermissionsVMPermissions :: Maybe GroupPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON GroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "groupPermissionsVM")
instance ToJSON GroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "groupPermissionsVM")


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
data InvitedUser = InvitedUser
  { invitedUserUserId :: Maybe Text -- ^ 
  , invitedUserInvitedAt :: Maybe UTCTime -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON InvitedUser where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "invitedUser")
instance ToJSON InvitedUser where
  toJSON = genericToJSON (removeFieldLabelPrefix False "invitedUser")


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


-- | 
newtype ProblemDetails = ProblemDetails { unProblemDetails :: (Map.Map Text Value) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic, Data)

-- | 
data RegisterUserVM = RegisterUserVM
  { registerUserVMId :: Maybe Text -- ^ 
  , registerUserVMSubscriptions :: Maybe [Text] -- ^ 
  , registerUserVMGroups :: Maybe [Text] -- ^ 
  , registerUserVMAdminPermission :: Maybe AdminPermission -- ^ 
  , registerUserVMName :: Maybe Text -- ^ 
  , registerUserVMUsername :: Maybe Text -- ^ 
  , registerUserVMEmail :: Maybe Text -- ^ 
  , registerUserVMPassword :: Maybe Text -- ^ 
  , registerUserVMIsAdmin :: Maybe Bool -- ^ 
  , registerUserVMProvider :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RegisterUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "registerUserVM")
instance ToJSON RegisterUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "registerUserVM")


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
  { renameSubscriptionVMName :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON RenameSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "renameSubscriptionVM")
instance ToJSON RenameSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "renameSubscriptionVM")


-- | 
data ReportCreateAdminVM = ReportCreateAdminVM
  { reportCreateAdminVMOwnerId :: Maybe Text -- ^ 
  , reportCreateAdminVMParentId :: Maybe Text -- ^ 
  , reportCreateAdminVMTemplateId :: Maybe Text -- ^ 
  , reportCreateAdminVMName :: Maybe Text -- ^ 
  , reportCreateAdminVMTags :: Maybe [Text] -- ^ 
  , reportCreateAdminVMIcon :: Maybe Text -- ^ 
  , reportCreateAdminVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON ReportCreateAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportCreateAdminVM")
instance ToJSON ReportCreateAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportCreateAdminVM")


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
data TemplateCreateAdminVM = TemplateCreateAdminVM
  { templateCreateAdminVMOwnerId :: Maybe Text -- ^ 
  , templateCreateAdminVMParentId :: Maybe Text -- ^ 
  , templateCreateAdminVMName :: Maybe Text -- ^ 
  , templateCreateAdminVMTags :: Maybe [Text] -- ^ 
  , templateCreateAdminVMIcon :: Maybe Text -- ^ 
  , templateCreateAdminVMContent :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TemplateCreateAdminVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "templateCreateAdminVM")
instance ToJSON TemplateCreateAdminVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "templateCreateAdminVM")


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


-- | 
data UpdateDataSourceConnectionStringVM = UpdateDataSourceConnectionStringVM
  { updateDataSourceConnectionStringVMConnectionString :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceConnectionStringVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceConnectionStringVM")
instance ToJSON UpdateDataSourceConnectionStringVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceConnectionStringVM")


-- | 
data UpdateDataSourcePermissionsVM = UpdateDataSourcePermissionsVM
  { updateDataSourcePermissionsVMNewPermissions :: DataSourcePermissions -- ^ 
  , updateDataSourcePermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourcePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourcePermissionsVM")
instance ToJSON UpdateDataSourcePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourcePermissionsVM")


-- | 
data UpdateDataSourceSubscriptionVM = UpdateDataSourceSubscriptionVM
  { updateDataSourceSubscriptionVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceSubscriptionVM")
instance ToJSON UpdateDataSourceSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceSubscriptionVM")


-- | 
data UpdateDataSourceVM = UpdateDataSourceVM
  { updateDataSourceVMName :: Maybe Text -- ^ 
  , updateDataSourceVMSubscriptionId :: Maybe Text -- ^ 
  , updateDataSourceVMConnectionString :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDataSourceVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDataSourceVM")
instance ToJSON UpdateDataSourceVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDataSourceVM")


-- | 
data UpdateDefaultPermissionsVM = UpdateDefaultPermissionsVM
  { updateDefaultPermissionsVMFilePermissions :: Maybe FilePermissions -- ^ 
  , updateDefaultPermissionsVMGroupPermissions :: Maybe GroupPermissions -- ^ 
  , updateDefaultPermissionsVMDataSourcePermissions :: Maybe DataSourcePermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateDefaultPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateDefaultPermissionsVM")
instance ToJSON UpdateDefaultPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateDefaultPermissionsVM")


-- | 
data UpdateFilePermissionsVM = UpdateFilePermissionsVM
  { updateFilePermissionsVMNewPermissions :: FilePermissions -- ^ 
  , updateFilePermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateFilePermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateFilePermissionsVM")
instance ToJSON UpdateFilePermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateFilePermissionsVM")


-- | 
data UpdateGroupPermissionsVM = UpdateGroupPermissionsVM
  { updateGroupPermissionsVMNewPermissions :: GroupPermissions -- ^ 
  , updateGroupPermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupPermissionsVM")
instance ToJSON UpdateGroupPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupPermissionsVM")


-- | 
data UpdateGroupVM = UpdateGroupVM
  { updateGroupVMName :: Maybe Text -- ^ 
  , updateGroupVMSubscriptionId :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateGroupVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateGroupVM")
instance ToJSON UpdateGroupVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateGroupVM")


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
  , updateSubscriptionPermissionsVMAdministrate :: Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionPermissionsVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionPermissionsVM")
instance ToJSON UpdateSubscriptionPermissionsVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionPermissionsVM")


-- | 
data UpdateSubscriptionPlanVM = UpdateSubscriptionPlanVM
  { updateSubscriptionPlanVMIsActive :: Maybe Bool -- ^ 
  , updateSubscriptionPlanVMDisplayName :: Maybe Text -- ^ 
  , updateSubscriptionPlanVMTimePeriodType :: Maybe Text -- ^ 
  , updateSubscriptionPlanVMTimePeriod :: Maybe Int -- ^ 
  , updateSubscriptionPlanVMTemplatesSpaceLimit :: Maybe Integer -- ^ 
  , updateSubscriptionPlanVMReportsSpaceLimit :: Maybe Integer -- ^ 
  , updateSubscriptionPlanVMExportsSpaceLimit :: Maybe Integer -- ^ 
  , updateSubscriptionPlanVMFileUploadSizeLimit :: Maybe Integer -- ^ 
  , updateSubscriptionPlanVMDataSourceLimit :: Maybe Int -- ^ 
  , updateSubscriptionPlanVMMaxUsersCount :: Maybe Int -- ^ 
  , updateSubscriptionPlanVMHasSpaceOverdraft :: Maybe Bool -- ^ 
  , updateSubscriptionPlanVMGroupLimit :: Maybe Int -- ^ 
  , updateSubscriptionPlanVMOnlineDesigner :: Maybe Bool -- ^ 
  , updateSubscriptionPlanVMIsDemo :: Maybe Bool -- ^ 
  , updateSubscriptionPlanVMUrlToBuy :: Maybe Text -- ^ 
  , updateSubscriptionPlanVMUnlimitedPage :: Maybe Bool -- ^ 
  , updateSubscriptionPlanVMPageLimit :: Maybe Int -- ^ 
  , updateSubscriptionPlanVMReadonlyTimeLimitType :: Maybe Text -- ^ 
  , updateSubscriptionPlanVMReadonlyTimeLimit :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionPlanVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionPlanVM")
instance ToJSON UpdateSubscriptionPlanVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionPlanVM")


-- | 
data UpdateSubscriptionVM = UpdateSubscriptionVM
  { updateSubscriptionVMName :: Maybe Text -- ^ 
  , updateSubscriptionVMLocale :: Maybe Text -- ^ 
  , updateSubscriptionVMDefaultPermissions :: Maybe DefaultPermissions -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateSubscriptionVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateSubscriptionVM")
instance ToJSON UpdateSubscriptionVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateSubscriptionVM")


-- | 
data UpdateUserVM = UpdateUserVM
  { updateUserVMSubscriptions :: Maybe [Text] -- ^ 
  , updateUserVMGroups :: Maybe [Text] -- ^ 
  , updateUserVMAdminPermission :: Maybe AdminPermission -- ^ 
  , updateUserVMName :: Maybe Text -- ^ 
  , updateUserVMUsername :: Maybe Text -- ^ 
  , updateUserVMEmail :: Maybe Text -- ^ 
  , updateUserVMPassword :: Maybe Text -- ^ 
  , updateUserVMIsAdmin :: Maybe Bool -- ^ 
  , updateUserVMProvider :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UpdateUserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "updateUserVM")
instance ToJSON UpdateUserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "updateUserVM")


-- | 
data UserProfileUpdateVM = UserProfileUpdateVM
  { userProfileUpdateVMName :: Maybe Text -- ^ 
  , userProfileUpdateVMUsername :: Maybe Text -- ^ 
  , userProfileUpdateVMEmail :: Maybe Text -- ^ 
  , userProfileUpdateVMPasswordNew :: Maybe Text -- ^ 
  , userProfileUpdateVMPasswordNew2 :: Maybe Text -- ^ 
  , userProfileUpdateVMSettings :: Maybe UserSettings -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserProfileUpdateVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userProfileUpdateVM")
instance ToJSON UserProfileUpdateVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userProfileUpdateVM")


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
data UserSettings = UserSettings
  { userSettingsProfileVisibility :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserSettings where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userSettings")
instance ToJSON UserSettings where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userSettings")


-- | 
data UserVM = UserVM
  { userVMId :: Maybe Text -- ^ 
  , userVMSubscriptions :: Maybe [Text] -- ^ 
  , userVMGroups :: Maybe [Text] -- ^ 
  , userVMAdminPermission :: Maybe AdminPermission -- ^ 
  , userVMIsAdmin :: Maybe Bool -- ^ 
  , userVMName :: Maybe Text -- ^ 
  , userVMUsername :: Maybe Text -- ^ 
  , userVMEmail :: Maybe Text -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UserVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userVM")
instance ToJSON UserVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userVM")


-- | 
data UsersVM = UsersVM
  { usersVMUsers :: Maybe [UserVM] -- ^ 
  , usersVMCount :: Maybe Integer -- ^ 
  , usersVMSkip :: Maybe Int -- ^ 
  , usersVMTake :: Maybe Int -- ^ 
  } deriving (Show, Eq, Generic, Data)

instance FromJSON UsersVM where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "usersVM")
instance ToJSON UsersVM where
  toJSON = genericToJSON (removeFieldLabelPrefix False "usersVM")


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
