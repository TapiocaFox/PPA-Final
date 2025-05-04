MPL=../mpl-v053/bin/mlton-mpl

closest-point: closest-point.sml closest-point.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-point closest-point.mlb

closest-point-parallel: closest-point-parallel.sml closest-point-parallel.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-point-parallel closest-point-parallel.mlb

closest-point-spatial: closest-point-spatial.sml closest-point-spatial.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-point-spatial closest-point-spatial.mlb

closest-point-spatial-parallel: closest-point-spatial-parallel.sml closest-point-spatial-parallel.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-point-spatial-parallel closest-point-spatial-parallel.mlb

py-sml-test: py-sml-test.sml py-sml-test.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../py-sml-test py-sml-test.mlb

clean:
	rm -f ../py-sml-test ../closest-point