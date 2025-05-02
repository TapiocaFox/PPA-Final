MPL=../mpl-v053/bin/mlton-mpl
MLB=py-sml-test.mlb

closest-points: *.sml *.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../closest-points $(MLB)

py-sml-test: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	$(MPL) -default-type int64 -default-type word64 -output ../py-sml-test $(MLB)

clean:
	rm -f ../py-sml-test 