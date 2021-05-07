

LC_ALL=C.UTF-8 R -e 'shiny::runApp(appDir = "/opt/credit",port = 8888,host = "0.0.0.0")' >> log 2>&1 &