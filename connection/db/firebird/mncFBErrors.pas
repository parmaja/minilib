{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

unit mncFBErrors;

interface

uses
  SysUtils, Classes, mncFBTypes, mncConnections, mncFBStrings;

{$i mncFBErrors.inc}

type
  EFBError = class(EmncException)
  private
    FSQLCode: Long;
    FErrorCode: Long;
  public
    constructor Create(ASQLCode: Long; Msg: string); overload;
    constructor Create(ASQLCode: Long; AErrorCode: Long; Msg: string); overload;
    property SQLCode: Long read FSQLCode;
    property ErrorCode: Long read FErrorCode;
  end;

  EFBExceptionError = class(EFBError)
  private
    FExceptionID: Integer;
    FExceptionMsg: string;
    FExceptionName: string;
  public
    constructor Create(ASQLCode: Long; AErrorCode: Long; AExceptionID: Integer; AExceptionName, AExceptionMsg:string; Msg: string); overload;
    property ExceptionID: Integer read FExceptionID;
    property ExceptionName: string read FExceptionName;
    property ExceptionMsg: string read FExceptionMsg;
  end;

  EFBRoleError = class(EFBError);
  EFBClientError = class(EFBError);
  EFBPlanError = class(EFBError);

  TFBDataBaseErrorMessage = (ShowSQLCode, ShowFBMessage, ShowSQLMessage);
  TFBDataBaseErrorMessages = set of TFBDataBaseErrorMessage;

  TFBError = (
    fbceUnknownError,
    fbceFirebirdInstallMissing,
    fbceNotSupported,
    fbceNotPermitted,
    fbceFileAccessError,
    fbceConnectionTimeout,
    fbceCannotSetDatabase,
    fbceCannotSetTransaction,
    fbceOperationCancelled,
    fbceDPBConstantNotSupported,
    fbceDPBConstantUnknown,
    fbceTPBConstantNotSupported,
    fbceTPBConstantUnknown,
    fbceDatabaseClosed,
    fbceDatabaseOpen,
    fbceDatabaseNameMissing,
    fbceNotInTransaction,
    fbceInTransaction,
    fbceTimeoutNegative,
    fbceUpdateWrongDB,
    fbceUpdateWrongTR,
    fbceDatabaseNotAssigned,
    fbceTransactionNotAssigned,
    fbceXSQLDAIndexOutOfRange,
    fbceXSQLDANameDoesNotExist,
    fbceEOF,
    fbceBOF,
    fbceInvalidStatementHandle,
    fbceSQLOpen,
    fbceSQLClosed,
    fbceDatasetOpen,
    fbceDatasetClosed,
    fbceUnknownSQLDataType,
    fbceInvalidColumnIndex,
    fbceInvalidParamColumnIndex,
    fbceInvalidDataConversion,
    fbceColumnIsNotNullable,
    fbceBlobCannotBeRead,
    fbceBlobCannotBeWritten,
    fbceEmptyQuery,
    fbceCannotOpenNonSQLSelect,
    fbceNoFieldAccess,
    fbceFieldReadOnly,
    fbceFieldNotFound,
    fbceNotEditing,
    fbceCannotInsert,
    fbceCannotPost,
    fbceCannotUpdate,
    fbceCannotDelete,
    fbceCannotRefresh,
    fbceBufferNotSet,
    fbceCircularReference,
    fbceSQLParseError,
    fbceUserAbort,
    fbceDataSetUniDirectional,
    fbceCannotCreateSharedResource,
    fbceWindowsAPIError,
    fbceColumnListsDontMatch,
    fbceColumnTypesDontMatch,
    fbceFieldUnsupportedType,
    fbceCircularDataLink,
    fbceEmptySQLStatement,
    fbceIsASelectStatement,
    fbceRequiredParamNotSet,
    fbceNoStoredProcName,
    fbceIsAExecuteProcedure,
    fbceUpdateFailed,
    fbceNotCachedUpdates,
    fbceNotLiveRequest,
    fbceNoProvider,
    fbceNoRecordsAffected,
    fbceNoTableName,
    fbceCannotCreatePrimaryIndex,
    fbceCannotDropSystemIndex,
    fbceTableNameMismatch,
    fbceIndexFieldMissing,
    fbceInvalidCancellation,
    fbceInvalidEvent,
    fbceMaximumEvents,
    fbceNoEventsRegistered,
    fbceInvalidQueueing,
    fbceInvalidRegistration,
    fbceInvalidBatchMove,
    fbceSQLDialectInvalid,
    fbceSPBConstantNotSupported,
    fbceSPBConstantUnknown,
    fbceServiceActive,
    fbceServiceInActive,
    fbceServerNameMissing,
    fbceQueryParamsError,
    fbceStartParamsError,
    fbceOutputParsingError,
    fbceUseSpecificProcedures,
    fbceSQLMonitorAlreadyPresent,
    fbceCantPrintValue,
    fbceEOFReached,
    fbceEOFInComment,
    fbceEOFInString,
    fbceParamNameExpected,
    fbceSuccess,
    fbceException,
    fbceNoOptionsSet,
    fbceNoDestinationDirectory,
    fbceNosourceDirectory,
    fbceNoUninstallFile,
    fbceOptionNeedsClient,
    fbceOptionNeedsServer,
    fbceInvalidOption,
    fbceInvalidOnErrorResult,
    fbceInvalidOnStatusResult,
    fbceDPBConstantUnknownEx,
    fbceTPBConstantUnknownEx,
    fbceUnknownPlan,
    fbceFieldSizeMismatch,
    fbceEventAlreadyRegistered,
    fbceStringTooLarge
    );

const
  FBErrorMessages: array[TFBError] of string = (
    SUnknownError,
    SFirebirdInstallMissing,
    SNotSupported,
    SNotPermitted,
    SFileAccessError,
    SConnectionTimeout,
    SCannotSetDatabase,
    SCannotSetTransaction,
    SOperationCancelled,
    SDPBConstantNotSupported,
    SDPBConstantUnknown,
    STPBConstantNotSupported,
    STPBConstantUnknown,
    SDatabaseClosed,
    SDatabaseOpen,
    SDatabaseNameMissing,
    SNotInTransaction,
    SInTransaction,
    STimeoutNegative,
    SUpdateWrongDB,
    SUpdateWrongTR,
    SDatabaseNotAssigned,
    STransactionNotAssigned,
    SXSQLDAIndexOutOfRange,
    SXSQLDANameDoesNotExist,
    SEOF,
    SBOF,
    SInvalidStatementHandle,
    SSQLOpen,
    SSQLClosed,
    SDatasetOpen,
    SDatasetClosed,
    SUnknownSQLDataType,
    SInvalidColumnIndex,
    SInvalidParamColumnIndex,
    SInvalidDataConversion,
    SColumnIsNotNullable,
    SBlobCannotBeRead,
    SBlobCannotBeWritten,
    SEmptyQuery,
    SCannotOpenNonSQLSelect,
    SNoFieldAccess,
    SFieldReadOnly,
    SFieldNotFound,
    SNotEditing,
    SCannotInsert,
    SCannotPost,
    SCannotUpdate,
    SCannotDelete,
    SCannotRefresh,
    SBufferNotSet,
    SCircularReference,
    SSQLParseError,
    SUserAbort,
    SDataSetUniDirectional,
    SCannotCreateSharedResource,
    SWindowsAPIError,
    SColumnListsDontMatch,
    SColumnTypesDontMatch,
    SFieldUnsupportedType,
    SCircularDataLink,
    SEmptySQLStatement,
    SIsASelectStatement,
    SRequiredParamNotSet,
    SNoStoredProcName,
    SIsAExecuteProcedure,
    SUpdateFailed,
    SNotCachedUpdates,
    SNotLiveRequest,
    SNoProvider,
    SNoRecordsAffected,
    SNoTableName,
    SCannotCreatePrimaryIndex,
    SCannotDropSystemIndex,
    STableNameMismatch,
    SIndexFieldMissing,
    SInvalidCancellation,
    SInvalidEvent,
    SMaximumEvents,
    SNoEventsRegistered,
    SInvalidQueueing,
    SInvalidRegistration,
    SInvalidBatchMove,
    SSQLDialectInvalid,
    SSPBConstantNotSupported,
    SSPBConstantUnknown,
    SServiceActive,
    SServiceInActive,
    SServerNameMissing,
    SQueryParamsError,
    SStartParamsError,
    SOutputParsingError,
    SUseSpecificProcedures,
    SSQLMonitorAlreadyPresent,
    SCantPrintValue,
    SEOFReached,
    SEOFInComment,
    SEOFInString,
    SParamNameExpected,
    SSuccess,
    SException,
    SNoOptionsSet,
    SNoDestinationDirectory,
    SNosourceDirectory,
    SNoUninstallFile,
    SOptionNeedsClient,
    SOptionNeedsServer,
    SInvalidOption,
    SInvalidOnErrorResult,
    SInvalidOnStatusResult,
    SDPBConstantUnknownEx,
    STPBConstantUnknownEx,
    SUnknownPlan,
    SFieldSizeMismatch,
    SEventAlreadyRegistered,
    SStringTooLarge
    );

implementation

{ EFBError }

constructor EFBError.Create(ASQLCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
end;

constructor EFBError.Create(ASQLCode: Long; AErrorCode: Long; Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := ASQLCode;
  FErrorCode := AErrorCode;
end;

{ EFBExceptionError }

constructor EFBExceptionError.Create(ASQLCode: Long; AErrorCode: Long; AExceptionID: Integer; AExceptionName, AExceptionMsg:string; Msg: string);
begin
  inherited Create(ASQLCode, AErrorCode, Msg);
  FExceptionID := AExceptionID;
  FExceptionName := AExceptionName;
  FExceptionMsg := AExceptionMsg;
end;

end.
 
