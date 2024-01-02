	db DEX_QWILFISH ; pokedex id

	db  65,  95,  75, 85,  55
	;   hp  atk  def  spd  spc

	db WATER, POISON ; type
	db 255 ; catch rate
	db 216 ; base exp

	INCBIN "gfx/pokemon/front/qwilfish.pic", 0, 1 ; sprite dimensions
	dw QwilfishPicFront, QwilfishPicBack

	db TACKLE, POISON_STING, NO_MOVE, NO_MOVE ; level 1 learnset
	db GROWTH_MEDIUM_FAST ; growth rate

	; tm/hm learnset
	tmhm WATER_GUN,    TOXIC,        TAKE_DOWN,          						\
	     HYPER_BEAM,  THUNDER_WAVE,   ICE_BEAM,     DOUBLE_EDGE,			    \
	     DOUBLE_TEAM, REST, SURF,      BLIZZARD 
	; end

	db 0 ; padding
