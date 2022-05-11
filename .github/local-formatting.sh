# Extensions necessary to tell fourmolu about 
EXTENSIONS="-o -XTypeApplications -o -XImportQualifiedPost"
fourmolu --mode inplace --check-idempotence $EXTENSIONS $(fd -ehs)
