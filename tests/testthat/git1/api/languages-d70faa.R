structure(list(method = "GET", url = "api/languages?type=source", 
    status_code = 403L, headers = structure(list(date = "Tue, 05 Dec 2023 12:44:02 GMT", 
        `access-control-allow-origin` = "*", `strict-transport-security` = "max-age=63072000; includeSubDomains; preload", 
        `server-timing` = "l7_lb_tls;dur=117, l7_lb_idle;dur=0, l7_lb_receive;dur=0, l7_lb_total;dur=130", 
        `access-control-expose-headers` = "Server-Timing"), class = "httr2_headers"), 
    body = raw(0), request = structure(list(url = "https://api.deepl.com/v2/languages?type=source", 
        method = "GET", headers = structure(list(Authorization = "DeepL-Auth-Key lalala"), redact = character(0)), 
        body = NULL, fields = list(), options = list(), policies = list(
            retry_max_tries = 3)), class = "httr2_request"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
