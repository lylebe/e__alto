curl --verbose \
  -H "Content-Type: application/alto-endpointcostparams+json" \
  -H "Accept: application/alto-endpointcost+json" \
  --data @$1 \
  -X POST http://localhost:8080/epcs
