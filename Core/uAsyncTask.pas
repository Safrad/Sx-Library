unit uAsyncTask;

interface

uses
  uSxThread;

type
  TAsyncTask = class
  public
    Thread: TSxThread;
    procedure Execute; virtual; abstract;
  end;

implementation

end.
 
