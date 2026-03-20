{*******************************************************************************

Scanner tolkens

*******************************************************************************}

module tolkens;

type

{ scanner input tolkens }
tolken = (c_undefined,  { undefined (must be first tolken) }
          c_plus,       { + }
          c_minus,      { - }
          c_times,      { * }
          c_rdiv,       { / }
          c_equ,        { = }
          c_nequ,       { <> }
          c_nequa,      { >< }
          c_ltn,        { < }
          c_gtn,        { > }
          c_lequ,       { <= }
          c_lequa,      { =< }
          c_gequ,       { >= }
          c_gequa,      { => }
          c_lparen,     { ( }
          c_rparen,     { ) }
          c_lbrkt,      { [ }
          c_rbrkt,      { ] }
          c_lct,        { left comment }
          c_rct,        { right comment }
          c_bcms,       { := }
          c_period,     { . }
          c_cma,        { , }
          c_scn,        { ; }
          c_cln,        { : }
          c_cmf,        { ^ }
          c_range,      { .. }
          c_hex,        { $ }
          c_oct,        { & }
          c_bin,        { % }
          c_num,        { # }
          c_linc,       { ! }
          c_div,       { div }
          c_mod,       { mod }
          c_nil,       { nil }
          c_in,        { in }
          c_or,        { or }
          c_and,       { and }
          c_xor,       { xor }
          c_not,       { not }
          c_if,        { if }
          c_then,      { then }
          c_else,      { else }
          c_case,      { case }
          c_of,        { of }
          c_repeat,    { repeat }
          c_until,     { until }
          c_while,     { while }
          c_do,        { do }
          c_for,       { for }
          c_to,        { to }
          c_downto,    { downto }
          c_begin,     { begin }
          c_end,       { end }
          c_with,      { with }
          c_goto,      { goto }
          c_const,     { const }
          c_var,       { var }
          c_type,      { type }
          c_array,     { array }
          c_record,    { record }
          c_set,       { set }
          c_file,      { file }
          c_function,  { function }
          c_procedure, { procedure }
          c_label,     { label }
          c_packed,    { packed }
          c_program,   { program }
          c_forward,   { forward }
          c_module,    { module }
          c_uses,      { uses }
          c_private,   { private }
          c_external,  { external }
          c_cexternal, { cexternal }
          c_mexternal, { mexternal }
          c_view,      { view }
          c_fixed,     { fixed }
          c_process,   { process }
          c_monitor,   { monitor }
          c_share,     { share }
          c_class,     { class }
          c_is,        { is }
          c_atom,      { atom }
          c_overload,  { overload }
          c_override,  { override }
          c_reference, { reference }
          c_thread,    { thread }
          c_joins,     { joins }
          c_static,    { static }
          c_inherited, { inherited }
          c_self,      { self }
          c_virtual,   { virtual }
          c_try,       { try}
          c_except,    { except }
          c_extends,   { extends }
          c_on,        { on }
          c_result,    { result }
          c_operator,  { operator }
          c_task,      { task}
          c_property,  { property }
          c_channel,   { channel }
          c_stream,    { stream }
          c_out,       { out }
          c_integer,    { unsigned integer constant }
          c_identifier, { identifier }
          c_string,     { string constant }
          c_real,       { real constant }
          c_eof);       { end of file (must be last tolken) }
tlkset = set of tolken; { tolken set }

begin
end.
