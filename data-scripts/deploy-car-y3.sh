cd /home/ben/trec-car/data/car-y3

rm -R /home/ben/trec-car/data/car-y3/*

tqaRawData="../tqa/train/tqa_v1_train.json"
y3json="/home/ben/trec-car/mediawiki-annotate/data-scripts/tqa-heading-cast-and-more-6.json"

$bin/tqa-import2 -o tqa2.cbor ${tqaRawData} --introduction-only


$bin/tqa-y3-export -o benchmarkY3test.cbor-outlines.cbor ${y3json} tqa2.cbor -T -s -c benchmarkY3-Y2-test-compat.json
$bin/tqa-y3-export -o benchmarkY3test.cbor-notes.cbor ${y3json} tqa2.cbor -T -n
$bin/tqa-y3-export -o benchmarkY3test.cbor ${y3json} tqa2.cbor -T -r


$bin/tqa-y3-export -o benchmarkY3train.cbor-outlines.cbor ${y3json} tqa2.cbor -t -s -c benchmarkY3-Y2-train-compat.json
$bin/tqa-y3-export -o benchmarkY3train.cbor-notes.cbor ${y3json} tqa2.cbor -t -n
$bin/tqa-y3-export -o benchmarkY3train.cbor ${y3json} tqa2.cbor -t -r




for f in *cbor; do
	$bin/trec-car-build-toc stubs  ${f}
	$bin/trec-car-build-toc pages  ${f}
	$bin/trec-car-build-toc page-names  ${f}
	$bin/trec-car-build-toc page-redirects  ${f}
done

mkdir benchmarkY3train
cp /home/ben/trec-car/mediawiki-annotate/data-scripts/README.mkd benchmarkY3train/
cp benchmarkY3train.cbor-outlines.cbor benchmarkY3train/
cp benchmarkY3train.cbor benchmarkY3train/
cp benchmarkY3-Y2-train-compat.json benchmarkY3train/


mkdir benchmarkY3test.public
cp /home/ben/trec-car/mediawiki-annotate/data-scripts/README.mkd benchmarkY3test.public/
cp benchmarkY3test.cbor-outlines.cbor benchmarkY3test.public/

tar -zcvf benchmarkY3train.tar.gz benchmarkY3train/
tar -zcvf benchmarkY3test.public.tar.gz benchmarkY3test.public/

