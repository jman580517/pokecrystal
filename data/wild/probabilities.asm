MACRO mon_prob
; percent, index
	db \1, \2 * 2
ENDM

GrassMonProbTable:
	table_width 2
	mon_prob 20,  0 ; 30,  0 ; 30% chance
	mon_prob 40,  1 ; 60,  1 ; 30% chance
	mon_prob 60,  2 ; 80,  2 ; 20% chance
	mon_prob 70,  3 ; 90,  3 ; 10% chance
	mon_prob 80,  4 ; 95,  4 ;  5% chance
	mon_prob 90,  5 ; 99,  5 ;  4% chance
	mon_prob 100, 6 ; 100, 6 ;  1% chance
	assert_table_length NUM_GRASSMON

WaterMonProbTable:
	table_width 2
	mon_prob 50,  0 ; 50% chance
	mon_prob 80,  1 ; 30% chance
	mon_prob 100, 2 ; 20% chance
	assert_table_length NUM_WATERMON
