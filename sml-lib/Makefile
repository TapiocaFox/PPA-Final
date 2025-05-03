MPL=../mpl-v053/bin/mlton-mpl

closest-points: closest-points.sml closest-points.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-points closest-points.mlb

py-sml-test: py-sml-test.sml py-sml-test.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../py-sml-test py-sml-test.mlb

clean:
	rm -f ../py-sml-test ../closest-points