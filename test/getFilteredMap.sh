curl --verbose \
  -H "Content-Type: application/alto-networkmapfilter+json" \
  -H "Accept: application/alto-networkmap+json" \
  --data @$1 \
  -X POST http://localhost:8080/networkmap
