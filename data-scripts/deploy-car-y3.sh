$bin/tqa-import2 -o tqa2.cbor train/tqa_v1_train.json 


$bin/tqa-y3-export -o tqa-y3-test-outlines.cbor tqa-heading-cast-and-more-6.json tqa2.cbor -t -s
$bin/tqa-y3-export -o tqa-y3-test-notes.cbor tqa-heading-cast-and-more-6.json tqa2.cbor -t -n
$bin/tqa-y3-export -o tqa-y3-test-pages.cbor tqa-heading-cast-and-more-6.json tqa2.cbor -t -r

for f in *cbor; do
	$bin/trec-car-build-toc stubs  ${f}
	$bin/trec-car-build-toc pages  ${f}
	$bin/trec-car-build-toc page-names  ${f}
	$bin/trec-car-build-toc page-redirects  ${f}

