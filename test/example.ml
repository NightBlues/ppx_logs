

let () =
  Logs.set_level ( Some Logs.Info );
  Logs.set_reporter ( Logs.format_reporter () );
  [%logs "const info"];
  [%logs "hello, %s; your number is %d" "Username" 123];
  [%logs.app "this is app %d" 1];
  [%logs.err "this is error %d" 1];
  [%logs.warn "this is warning %d" 1];
  [%logs.info "this is info %d" 1];
  [%logs.debug "this is debug %d" 1]
