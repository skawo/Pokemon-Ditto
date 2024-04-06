FixPP:
	ld a, 0
	ld [wWhichPokemon], a
	ld [wMonDataLocation], a
	ld [wUnusedD153], a
	
	ld hl, wPartyMon1PP
	push hl

.loop
	ld a, 0
	ld [wUnusedD366], a
	
		.loopinner
			ld a, [wUnusedD366]
			ld [wCurrentMenuItem], a

			callfar GetMaxPP
		
			pop hl
		
			ld a, [wMaxPP]
			ld [hli],  a

			ld a, [wUnusedD366]
			inc a
			ld [wUnusedD366], a
			
			push hl
			
			cp 4
			jr nz, .loopinner
			
		.endinner
		
	pop hl
	dec hl
	dec hl
	dec hl
	dec hl

	ld bc, 44
	add hl, bc
	push hl
	
	ld a, [wUnusedD153]
	inc a	
	ld [wWhichPokemon], a
	ld [wUnusedD153], a
	
	cp 6
	jr nz, .loop
	
	pop hl
	ret


CopyOpponentParty:
; If SELECT held, don't copy
	ldh a, [hJoyHeld]
	ld b, SELECT
	and a, b
	jp nz, .dontcopy
	
.docopy
; Copy main data
	ld hl, wEnemyPartyCount
	ld bc, wEnemyMonOT - wEnemyPartyCount
	ld de, wPartyDataStart
	call CopyData
	
	ld a, 6
	ld b, a
	ld hl, wPartyMon1OTID
	
; Copy OT ID
.loopCopyOT
	ld a, [wPlayerID]
	ld [hl], a
	ld a, [wPlayerID + 1]
	inc hl
	ld [hl], a	
	dec hl
	
	ld de, wPartyMon2OTID - wPartyMon1OTID
	add hl, de
	
	dec b
	ld a, b
	cp 0
	jr nz, .loopCopyOT
	
; Copy Pokemon names	
	ld a, 6
	ld [wUnusedD366], a
	ld bc, wPartyMon1Species
	ld de, wPartyMon1Nick	

.loopFixNames
	push bc
	push de	
	
	ld a, [bc]
	ld [wd11e], a
	call GetMonName
	
	pop de
	pop bc
	push bc
	push de	
	
	ld bc, 10
	ld hl, wcd6d
	call CopyData
	
	pop de
	
	ld hl, 0
	add hl, de
	ld de, wPartyMon2Nick - wPartyMon1Nick
	add hl, de
	ld d, h
	ld e, l
	
	pop bc
	
	ld hl, 0
	add hl, bc	
	ld bc, wPartyMon2Species - wPartyMon1Species
	add hl, bc
	ld b, h
	ld c, l	
	
	ld a, [wUnusedD366]
	dec a
	ld [wUnusedD366], a
	cp 0
	jr nz, .loopFixNames
	
; Copy Trainer Name	

	ld a, 6
	ld [wUnusedD366], a
	ld de, wPartyMon1OT
	ld bc, wPartyMon2OT - wPartyMon1OT 
	
.loopCopyOTName
	
	push de 
	push bc 
	
	ld hl, wPlayerName
	ld bc, wPartyMon2OT - wPartyMon1OT
	call CopyData	
	
	pop bc
	pop de
	
	ld hl, 0
	add hl, bc	
	add hl, de
	ld d, h
	ld e, l

	ld a, [wUnusedD366]
	dec a
	ld [wUnusedD366], a
	cp 0
	jr nz, .loopCopyOTName

	call FixPP
	
.dontcopy
	ret

CopyWildMon:
; If SELECT held, don't copy
	ldh a, [hJoyHeld]
	ld b, SELECT
	and a, b
	jp nz, .dontcopywildmon


.docopywildmon
	ld a, 1
	ld hl, wPartyCount
	ld [hl], a
	
	ld a, 255
	ld hl, wPartySpecies + 1
	ld [hl], a
	
	ld a, [wEnemyMonSpecies]
	ld hl, wPartySpecies
	ld [hl], a
	
	ld a, [wEnemyMonHP]
	ld [wPartyMon1HP], a
	ld a, [wEnemyMonHP + 1]
	ld [wPartyMon1HP + 1], a	
	
	ld a, [wEnemyMonBoxLevel]
	ld hl, wPartyMon1BoxLevel
	ld [hl], a		
	
	ld hl, wEnemyMonStatus
	ld bc, wEnemyMonDVs - wEnemyMonStatus
	ld de, wPartyMon1Status
	call CopyData
	
	ld hl, wEnemyMonDVs
	ld bc, wEnemyMonLevel - wEnemyMonDVs
	ld de, wPartyMon1DVs
	call CopyData	
	
	ld a, [wEnemyMonLevel]
	ld hl, wPartyMon1Level
	ld [hl], a		
	
	ld hl, wEnemyMonMaxHP
	ld bc, wEnemyMonPP - wEnemyMonMaxHP
	ld de, wPartyMon1MaxHP
	call CopyData
	
	ld hl, wEnemyMonPP
	ld bc, wEnemyMonBaseStats - wEnemyMonPP
	ld de, wPartyMon1PP
	call CopyData

	ld a, [wEnemyMonSpecies]
	ld [wd11e], a
	call GetMonName
	ld bc, 10
	ld hl, wcd6d
	ld de, wPartyMon1Nick
	call CopyData	
	
	ld hl, wPlayerName
	ld bc, wPartyMon2OT - wPartyMon1OT
	ld de, wPartyMon1OT
	call CopyData	
	
	ld a, 0
	ld [wMonDataLocation], a
	ld [wWhichPokemon], a
	call LoadMonData
	
	ld a, [wPartyMon1Level]
	ld d, a
	callfar CalcExperience
	
	ld hl, wPartyMon1Exp
	ldh a, [hExperience]
	ld [hli], a
	ldh a, [hExperience + 1]
	ld [hli], a
	ldh a, [hExperience + 2]
	ld [hl], a		
	
	call FixPP
	
.dontcopywildmon
	ret

PrintBeginningBattleText:
	call CopyWildMon
	ld a, [wIsInBattle]
	dec a
	jr nz, .trainerBattle
	ld a, [wCurMap]
	cp POKEMON_TOWER_3F
	jr c, .notPokemonTower
	cp POKEMON_TOWER_7F + 1
	jr c, .pokemonTower
.notPokemonTower
	ld a, [wEnemyMonSpecies2]
	call PlayCry
	ld hl, WildMonAppearedText
	ld a, [wMoveMissed]
	and a
	jr z, .notFishing
	ld hl, HookedMonAttackedText
.notFishing
	jr .wildBattle
.trainerBattle
	call CopyOpponentParty
	call .playSFX
	ld c, 20
	call DelayFrames
	
	ld hl, TrainerWantsToFightText
	ld a, [wTrainerClass]
	cp 1
	jr nz, .wildBattle
	ld hl, StudentWantsToFightText
.wildBattle
	push hl
	callfar DrawAllPokeballs
	pop hl
	call PrintText
	jr .done
.pokemonTower
	ld b, SILPH_SCOPE
	call IsItemInBag
	ld a, [wEnemyMonSpecies2]
	ld [wcf91], a
	cp RESTLESS_SOUL
	jr z, .isMarowak
	ld a, b
	and a
	jr z, .noSilphScope
	callfar LoadEnemyMonData
	jr .notPokemonTower
.noSilphScope
	ld hl, EnemyAppearedText
	call PrintText
	ld hl, GhostCantBeIDdText
	call PrintText
	jr .done
.isMarowak
	ld a, b
	and a
	jr z, .noSilphScope
	ld hl, EnemyAppearedText
	call PrintText
	ld hl, UnveiledGhostText
	call PrintText
	callfar LoadEnemyMonData
	callfar MarowakAnim
	ld hl, WildMonAppearedText
	call PrintText

.playSFX
	xor a
	ld [wFrequencyModifier], a
	ld a, $80
	ld [wTempoModifier], a
	ld a, SFX_SILPH_SCOPE
	call PlaySound
	jp WaitForSoundToFinish
.done
	ret

WildMonAppearedText:
	text_far _WildMonAppearedText
	text_end

HookedMonAttackedText:
	text_far _HookedMonAttackedText
	text_end

EnemyAppearedText:
	text_far _EnemyAppearedText
	text_end

TrainerWantsToFightText:
	text_far _TrainerWantsToFightText
	text_end
	
StudentWantsToFightText:
	text_far _StudentWantsToFightText
	text_end	

UnveiledGhostText:
	text_far _UnveiledGhostText
	text_end

GhostCantBeIDdText:
	text_far _GhostCantBeIDdText
	text_end

PrintSendOutMonMessage:
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl]
	ld hl, GoText
	jr z, .printText
	xor a
	ldh [hMultiplicand], a
	ld hl, wEnemyMonHP
	ld a, [hli]
	ld [wLastSwitchInEnemyMonHP], a
	ldh [hMultiplicand + 1], a
	ld a, [hl]
	ld [wLastSwitchInEnemyMonHP + 1], a
	ldh [hMultiplicand + 2], a
	ld a, 25
	ldh [hMultiplier], a
	call Multiply
	ld hl, wEnemyMonMaxHP
	ld a, [hli]
	ld b, [hl]
	srl a
	rr b
	srl a
	rr b
	ld a, b
	ld b, 4
	ldh [hDivisor], a ; enemy mon max HP divided by 4
	call Divide
	ldh a, [hQuotient + 3] ; a = (enemy mon current HP * 25) / (enemy max HP / 4); this approximates the current percentage of max HP
	ld hl, GoText ; 70% or greater
	cp 70
	jr nc, .printText
	ld hl, DoItText ; 40% - 69%
	cp 40
	jr nc, .printText
	ld hl, GetmText ; 10% - 39%
	cp 10
	jr nc, .printText
	ld hl, EnemysWeakText ; 0% - 9%
.printText
	jp PrintText

GoText:
	text_far _GoText
	text_asm
	jr PrintPlayerMon1Text

DoItText:
	text_far _DoItText
	text_asm
	jr PrintPlayerMon1Text

GetmText:
	text_far _GetmText
	text_asm
	jr PrintPlayerMon1Text

EnemysWeakText:
	text_far _EnemysWeakText
	text_asm

PrintPlayerMon1Text:
	ld hl, PlayerMon1Text
	ret

PlayerMon1Text:
	text_far _PlayerMon1Text
	text_end

RetreatMon:
	ld hl, PlayerMon2Text
	jp PrintText

PlayerMon2Text:
	text_far _PlayerMon2Text
	text_asm
	push de
	push bc
	ld hl, wEnemyMonHP + 1
	ld de, wLastSwitchInEnemyMonHP + 1
	ld b, [hl]
	dec hl
	ld a, [de]
	sub b
	ldh [hMultiplicand + 2], a
	dec de
	ld b, [hl]
	ld a, [de]
	sbc b
	ldh [hMultiplicand + 1], a
	ld a, 25
	ldh [hMultiplier], a
	call Multiply
	ld hl, wEnemyMonMaxHP
	ld a, [hli]
	ld b, [hl]
	srl a
	rr b
	srl a
	rr b
	ld a, b
	ld b, 4
	ldh [hDivisor], a
	call Divide
	pop bc
	pop de
	ldh a, [hQuotient + 3] ; a = ((LastSwitchInEnemyMonHP - CurrentEnemyMonHP) / 25) / (EnemyMonMaxHP / 4)
; Assuming that the enemy mon hasn't gained HP since the last switch in,
; a approximates the percentage that the enemy mon's total HP has decreased
; since the last switch in.
; If the enemy mon has gained HP, then a is garbage due to wrap-around and
; can fall in any of the ranges below.
	ld hl, EnoughText ; HP stayed the same
	and a
	ret z
	ld hl, ComeBackText ; HP went down 1% - 29%
	cp 30
	ret c
	ld hl, OKExclamationText ; HP went down 30% - 69%
	cp 70
	ret c
	ld hl, GoodText ; HP went down 70% or more
	ret

EnoughText:
	text_far _EnoughText
	text_asm
	jr PrintComeBackText

OKExclamationText:
	text_far _OKExclamationText
	text_asm
	jr PrintComeBackText

GoodText:
	text_far _GoodText
	text_asm
	jr PrintComeBackText

PrintComeBackText:
	ld hl, ComeBackText
	ret

ComeBackText:
	text_far _ComeBackText
	text_end
