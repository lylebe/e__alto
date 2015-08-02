curl --verbose \
  -H "Content-Type: application/alto-costmapfilter+json" \
  -H "Accept: application/alto-costmap+json" \
  --data @$1 \
  -X POST http://localhost:8080/costmaps/1
