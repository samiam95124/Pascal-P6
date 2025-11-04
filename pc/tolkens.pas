{*******************************************************************************

Scanner tolkens

*******************************************************************************}

module tolkens;

type

{ scanner input tolkens }
tolken = (cundefined,  { undefined (must be first tolken) }
          cplus,       { + }
          cminus,      { - }
          ctimes,      { * }
          crdiv,       { / }
          cequ,        { = }
          cnequ,       { <> }
          cnequa,      { >< }
          cltn,        { < }
          cgtn,        { > }
          clequ,       { <= }
          clequa,      { =< }
          cgequ,       { >= }
          cgequa,      { => }
          clparen,     { ( }
          crparen,     { ) }
          clbrkt,      { [ }
          crbrkt,      { ] }
          clct,        { left comment }
          crct,        { right comment }
          cbcms,       { := }
          cperiod,     { . }
          ccma,        { , }
          cscn,        { ; }
          ccln,        { : }
          ccmf,        { ^ }
          crange,      { .. }
          chex,        { $ }
          coct,        { & }
          cbin,        { % }
          cnum,        { # }
          clinc,       { ! }
          cdiv,        { div }
          cmod,        { mod }
          cnil,        { nil }
          cin,         { in }
          cor,         { or }
          cand,        { and }
          cxor,        { xor }
          cnot,        { not }
          cif,         { if }
          cthen,       { then }
          celse,       { else }
          ccase,       { case }
          cof,         { of }
          crepeat,     { repeat }
          cuntil,      { until }
          cwhile,      { while }
          cdo,         { do }
          cfor,        { for }
          cto,         { to }
          cdownto,     { downto }
          cbegin,      { begin }
          cend,        { end }
          cwith,       { with }
          cgoto,       { goto }
          cconst,      { const }
          cvar,        { var }
          ctype,       { type }
          carray,      { array }
          crecord,     { record }
          cset,        { set }
          cfile,       { file }
          cfunction,   { function }
          cprocedure,  { procedure }
          clabel,      { label }
          cpacked,     { packed }
          cprogram,    { program }
          cforward,    { forward }
          cmodule,     { module }
          cuses,       { uses }
          cprivate,    { private }
          cexternal,   { external }
          cview,       { view }
          cfixed,      { fixed }
          cprocess,    { process }
          cmonitor,    { monitor }
          cshare,      { share }
          cclass,      { class }
          cis,         { is }
          catom,       { atom }
          coverload,   { overload }
          coverride,   { override }
          creference,  { reference }
          cthread,     { thread }
          cjoins,      { joins }
          cstatic,     { static }
          cinherited,  { inherited }
          cself,       { self }
          cvirtual,    { virtual }
          ctry,        { try}
          cexcept,     { except }
          cextends,    { extends }
          con,         { on }
          cresult,     { result }
          coperator,   { operator }
          ctask,       { task}
          cproperty,   { property }
          cchannel,    { channel }
          cstream,     { stream }
          cout,        { out }
          cinteger,    { unsigned integer constant }
          cidentifier, { identifier }
          cstring,     { string constant }
          creal,       { real constant }
          ceof);       { end of file (must be last tolken) }
tlkset = set of tolken; { tolken set }

begin
end.