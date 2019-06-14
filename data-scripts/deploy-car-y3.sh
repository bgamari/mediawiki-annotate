cd /home/ben/trec-car/data/car-y3

tqaRawData="../tqa/train/tqa_v1_train.json"
y3json="/home/ben/trec-car/mediawiki-annotate/data-scripts/tqa-heading-cast-and-more-6.json"

$bin/tqa-import2 -o tqa2.cbor ${tqaRawData}


$bin/tqa-y3-export -o tqa-y3-test-outlines.cbor ${y3json} tqa2.cbor -T -s -c tqa-y2-y3-test-compat.json
$bin/tqa-y3-export -o tqa-y3-test-notes.cbor ${y3json} tqa2.cbor -T -n
$bin/tqa-y3-export -o tqa-y3-test-pages.cbor ${y3json} tqa2.cbor -T -r


$bin/tqa-y3-export -o tqa-y3-train-outlines.cbor ${y3json} tqa2.cbor -t -s -c tqa-y2-y3-train-compat.json
$bin/tqa-y3-export -o tqa-y3-train-notes.cbor ${y3json} tqa2.cbor -t -n
$bin/tqa-y3-export -o tqa-y3-train-pages.cbor ${y3json} tqa2.cbor -t -r




for f in *cbor; do
	$bin/trec-car-build-toc stubs  ${f}
	$bin/trec-car-build-toc pages  ${f}
	$bin/trec-car-build-toc page-names  ${f}
	$bin/trec-car-build-toc page-redirects  ${f}
done

mkdir benchmarkY3train
cp /home/ben/trec-car/mediawiki-annotate/data-scripts/README.mkd benchmarkY3train/
cp tqa-y3-train-outlines.cbor benchmarkY3train/
cp tqa-y3-train-pages.cbor benchmarkY3train/
cp tqa-y2-y3-train-compat.json benchmarkY3train/


mkdir benchmarkY3test.public
cp /home/ben/trec-car/mediawiki-annotate/data-scripts/README.mkd benchmarkY3test.public/
cp tqa-y3-test-outlines.cbor benchmarkY3test.public/

tar -zcvf benchmarkY3train.tar.gz benchmarkY3train/
tar -zcvf benchmarkY3test.public.tar.gz benchmarkY3test.public/

