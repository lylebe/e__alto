curl --verbose \
  -H "Content-Type: application/alto-endpointpropparams+json" \
  -H "Accept: application/alto-endpointprop+json" \
  --data @$1 \
  -X POST http://localhost:8080/eps
