{ 

Language extension routines. These routines allow specification of semantic
functions beyond the base ISO 7185 specification. 

}

module extend;  

procedure capture;
begin
end;

function chkbrk: boolean;
begin
  chkbrk := false
end;

procedure exitprogram(ec: integer);
begin
  ec := ec
end;

begin
end.
