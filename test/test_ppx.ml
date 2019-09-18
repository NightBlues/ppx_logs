let%expect_test "main" =
  Logs.set_level ( Some Logs.Debug );
  let reporter = Logs.format_reporter ~pp_header:Logs.pp_header () in
  Logs.set_reporter reporter;
  [%logs "const info"];
  [%expect "[INFO]test/test_ppx.ml:5 const info"];
  [%logs "hello, %s; n = %d" "User" 123];
  [%expect "[INFO]test/test_ppx.ml:7 hello, User; n = 123"];
  [%logs.app "this is app %d" 1];
  [%expect "test/test_ppx.ml:9 this is app 1"];
  [%logs.err "this is error %d" 1];
  [%expect "[ERROR]test/test_ppx.ml:11 this is error 1"];
  [%logs.warn "this is warning %d" 1];
  [%expect "[WARNING]test/test_ppx.ml:13 this is warning 1"];
  [%logs.info "this is info %d" 1];
  [%expect "[INFO]test/test_ppx.ml:15 this is info 1"];
  [%logs.debug "this is debug %d" 1];
  [%expect "[DEBUG]test/test_ppx.ml:17 this is debug 1"]

