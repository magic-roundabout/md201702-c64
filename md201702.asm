;
; MD201702
;

; Code and graphics by T.M.R/Cosine
; Music by Odie/Cosine


; Select an output filename
		!to "md201702.prg",cbm


; Yank in binary data
		* = $2900
music		!binary "data/sporting_chance.prg",,2

		* = $4800
		!binary "data/copyright.chr"

		* = $4c00
		!binary "data/ninja_gaiden.chr"


; Constants
rstr1p		= $00
rstr2p		= $2c
rstr3p		= $7c
rstr4p		= $c8
rstr5p		= $f1

bg_colour	= $0c
bg_colour_m	= bg_colour+(bg_colour*$10)


; Labels
rn		= $50
sync		= $51

rt_store_1	= $52
rt_store_2	= $53

scroll_x	= $54
scroll_spd	= $55

cos_at_1	= $56		; for the FLI effect
cos_speed_1	= $57
preset_tmr	= $58		; two bytes used
colour_cnt	= $5a

cos_at_2	= $5c		; for the logos
cos_speed_2	= $02		; constant
cos_offset_2	= $30		; constant
cos_at_3	= $5d		; for the logos
cos_speed_3	= $03		; constant
cos_offset_3	= $43		; constant
cos_at_4	= $5e		; for the logos
cos_speed_4	= $fe		; constant
cos_offset_4	= $e0		; constant

cos_work_2	= $60		; two bytes used
cos_work_3	= $62		; two bytes used
cos_work_4	= $64		; two bytes used


logo_d016_m1	= $66
logo_d018_m1	= $67
logo_d021_m1	= $68
logo_d022_m1	= $69
logo_d023_m1	= $6a

logo_d016_m2	= $6b
logo_d018_m2	= $6c
logo_d021_m2	= $6d
logo_d022_m2	= $6e
logo_d023_m2	= $6f

logo_d016_m3	= $70
logo_d018_m3	= $71
logo_d021_m3	= $72
logo_d022_m3	= $73
logo_d023_m3	= $74

cos_offsets	= $80		; $21 bytes long

fli_colours	= $6004		; start of the effect's colour RAM

colour_table_1	= $ce00
colour_table_2	= $cf00


; Entry point at $8000
		* = $8000
entry		sei

		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Reset screen colours
		ldx #$00
		lda #$0b
set_colours	sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne set_colours

; Reset the FLI colours
		ldx #$00
		lda #bg_colour_m
set_fli_cols	sta $6000,x
		sta $6400,x
		sta $6800,x
		sta $6c00,x
		sta $7000,x
		sta $7400,x
		sta $7800,x
		sta $7c00,x
		sta $d800,x
		inx
		cpx #$a0
		bne set_fli_cols

; Build the FLI effect bitmap
		ldx #$00
		lda #%01001000		; skinny splits
;		lda #%01011010		; chunky splits
bitmap_build	sta $4020,x
		sta $4168,x
		sta $42b0,x
		sta $43f8,x
		inx
		cpx #$e0
		bne bitmap_build

; Set up the scrolling message's space on screen
		ldx #$00
scroll_init	lda #$80
		sta $5fc0,x
		lda scroll_cols,x
		sta $dbc0,x
		inx
		cpx #$27
		bne scroll_init


; Clear zero page workspaces
		ldx #$50
		lda #$00
zp_nuke		sta $00,x
		inx
		bne zp_nuke

		lda #$01
		sta rn


; Set up the first FLI effect preset
		jsr preset_reset
		jsr preset_fetch

; Wipe the FLI colour tables
		ldx #$00
fli_col_nuke	lda #bg_colour
		sta colour_table_1,x
		lda #(bg_colour*$10)
		sta colour_table_2,x
		inx
		bne fli_col_nuke


; Rest the scrolling message
		jsr reset
		lda #$03
		sta scroll_spd


; Initialise the music
		lda #$00
		jsr music+$00

		cli


; Small delay before the FLI effect starts
		ldx #$32
start_wait	jsr sync_wait
		dex
		bne start_wait


; Update the FLI effect until a new preset needs fetching
main_loop	jsr sync_wait

		jsr fli_update

; Update the colour tables and counter
		ldx colour_cnt
		lda colour_table,x
		sta colour_table_1,x
		asl
		asl
		asl
		asl
		sta colour_table_2,x
		inx
		stx colour_cnt

; Check to see if a new preset is needed
		ldx preset_tmr+$00
		dex
		cpx #$ff
		bne *+$04
		dec preset_tmr+$01
		stx preset_tmr+$00

		txa
		ora preset_tmr+$01
		beq preset_call

		jmp main_loop


; A new effect preset is needed so...
preset_call	ldx #$00
		stx colour_cnt

; Update the FLI effect until the colours are cleared out
main_loop_2	jsr fli_update

		ldx colour_cnt
		lda #bg_colour
		sta colour_table_1,x
		lda #(bg_colour*$10)
		sta colour_table_2,x
		inx
		stx colour_cnt
		cpx #$c0
		bne main_loop_2

; Fetch a new preset and jump back to the start up wait
		jsr preset_fetch

		jmp main_loop


; Unrolled code to update the FLI effect
fli_update	lda cos_at_1
		clc
		adc cos_speed_1
		sta cos_at_1
		tax

!set column_cnt=$00
!do {
		txa
		clc
		adc cos_offsets+$00+(column_cnt*$02)
		tax
		lda fli_cosinus,x
		sta rt_store_1

		txa
		clc
		adc cos_offsets+$01+(column_cnt*$02)
		tax
		stx rt_store_2
		ldy fli_cosinus,x

		ldx rt_store_1

!set line_cnt=$00
!do {
		lda colour_table_1+$00+(line_cnt*$08),y
		ora colour_table_2+$00+(line_cnt*$08),x
		sta fli_colours+$0000+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$01+(line_cnt*$08),y
		ora colour_table_2+$01+(line_cnt*$08),x
		sta fli_colours+$0400+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$02+(line_cnt*$08),y
		ora colour_table_2+$02+(line_cnt*$08),x
		sta fli_colours+$0800+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$03+(line_cnt*$08),y
		ora colour_table_2+$03+(line_cnt*$08),x
		sta fli_colours+$0c00+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$04+(line_cnt*$08),y
		ora colour_table_2+$04+(line_cnt*$08),x
		sta fli_colours+$1000+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$05+(line_cnt*$08),y
		ora colour_table_2+$05+(line_cnt*$08),x
		sta fli_colours+$1400+(line_cnt*$29)+(column_cnt)
		lda colour_table_1+$06+(line_cnt*$08),y
		ora colour_table_2+$06+(line_cnt*$08),x
		sta fli_colours+$1800+(line_cnt*$29)+(column_cnt)
		!if line_cnt<$03 {
		lda colour_table_1+$07+(line_cnt*$08),y
		ora colour_table_2+$07+(line_cnt*$08),x
		sta fli_colours+$1c00+(line_cnt*$29)+(column_cnt)
		}

		!set line_cnt=line_cnt+$01
} until line_cnt=$04

		ldx rt_store_2

		!set column_cnt=column_cnt+$01
} until column_cnt=$1c

		rts


; Reset the scrolling message
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts


; Read a block of preset data for the FLI effect ($80 is a "wrap" byte)
preset_fetch	ldx #$00
pf_loop		jsr preset_mread
		sta cos_offsets,x
		inx
		cpx #$38
		bne pf_loop

; Fetch the preset speed
		jsr preset_mread
		sta cos_speed_1

; Set the preset timer
		lda #$00
		sta preset_tmr+$00
		lda #$03
		sta preset_tmr+$01

		rts

; FLI preset reader self mod code
preset_mread	lda preset_data
		cmp #$80
		bne preset_okay
		jsr preset_reset
		jmp preset_mread

preset_okay	inc preset_mread+$01
		bne *+$05
		inc preset_mread+$02

		rts

; Reset the FLI preset reader's self mod code
preset_reset	lda #<preset_data
		sta preset_mread+$01
		lda #>preset_data
		sta preset_mread+$02
		rts


; Runtime sync wait
sync_wait	lda #$00
		sta sync

		cmp sync
		beq *-$02

		rts


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2

		cmp #$03
		bne *+$05
		jmp rout3

		cmp #$04
		bne *+$05
		jmp rout4

		cmp #$05
		bne *+$05
		jmp rout5


; Raster split 1
rout1		lda #bg_colour
		sta $d020
		sta $d021

		lda #$3b
		sta $d011
		lda #$10
		sta $d016
		lda #$80
		sta $d018

		lda #$c6
		sta $dd00

; Play the music
		jsr music+$03

; Set up for the next interrupt
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		jmp ea31


		* = ((*/$100)+$01)*$100

; Raster split 2
rout2		nop
		nop
		nop
		nop
		nop
		bit $ea

		lda $d012
		cmp #rstr2p+$01
		bne *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		nop
		lda $d012
		cmp #rstr2p+$02
		bne *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr2p+$03
		bne *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr2p+$04
		bne *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		bit $ea
		nop
		lda $d012
		cmp #rstr2p+$05
		bne *+$02
;		sta $d020

		nop
		nop
		nop

		ldx #$09
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr2p+$06
		bne *+$02
;		sta $d020

; FLI - first character line
		ldx #$0e
		dex
		bne *-$01
		bit $ea

		ldx #$3c
		ldy #$90
		lda #$11
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3d
		ldy #$a0
		lda #$12
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3e
		ldy #$b0
		lda #$13
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3f
		ldy #$c0
		lda #$14
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$38
		ldy #$d0
		lda #$15
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$39
		ldy #$e0
		lda #$16
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3a
		ldy #$f0
		lda #$17
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

; FLI - second and third character lines
!set line_cnt=$00
!do {
		ldx #$3b
		ldy #$80
		lda #$10
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3c
		ldy #$90
		lda #$11
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3d
		ldy #$a0
		lda #$12
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3e
		ldy #$b0
		lda #$13
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3f
		ldy #$c0
		lda #$14
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$38
		ldy #$d0
		lda #$15
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$39
		ldy #$e0
		lda #$16
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3a
		ldy #$f0
		lda #$17
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		!set line_cnt=line_cnt+$01
} until line_cnt=$02

; FLI - last character line
		ldx #$3b
		ldy #$80
		lda #$10
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3c
		ldy #$90
		lda #$11
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3d
		ldy #$a0
		lda #$12
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3e
		ldy #$b0
		lda #$13
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$3f
		ldy #$c0
		lda #$14
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$38
		ldy #$d0
		lda #$15
		sta $d016
		sty $d018
		stx $d011

		bit $ea
		nop

		ldx #$39
		ldy #$e0
		lda #$16
		sta $d016
		sty $d018
		stx $d011

;  Set a few video registers for the logos
		lda #$7b
		sta $d011
		lda logo_d016_m1
		sta $d016
		lda logo_d018_m1
		sta $d018

		lda logo_d021_m1
		sta $d021
		lda logo_d022_m1
		sta $d022
		lda logo_d023_m1
		sta $d023

		ldx #$04
		dex
		bne *-$01
		lda #$5b
		sta $d011


; Set up for the next interrupt
		lda #rstr3p
		sta $d012
		lda #$03
		sta rn

; Tell the runtime code to get going
		lda #$01
		sta sync

		jmp ea31


; Raster split 3
rout3		lda logo_d016_m2
		sta $d016
		lda logo_d018_m2
		sta $d018

		lda logo_d021_m2
		sta $d021
		lda logo_d022_m2
		sta $d022
		lda logo_d023_m2
		sta $d023

; Set up for the next interrupt
		lda #rstr4p
		sta $d012
		lda #$04
		sta rn

		jmp ea31


; Raster split 4
rout4		lda logo_d016_m3
		sta $d016
		lda logo_d018_m3
		sta $d018

		lda logo_d021_m3
		sta $d021
		lda logo_d022_m3
		sta $d022
		lda logo_d023_m3
		sta $d023

; Update the scrolling message
		ldy scroll_spd

scroll_loop	ldx scroll_x
		inx
		cpx #$08
		bne scr_xb

		ldx #$00
mover		lda $5fc1,x
		sta $5fc0,x
		inx
		cpx #$27
		bne mover

mread		lda scroll_text
		bne okay
		jsr reset
		jmp mread

; Check for a speed command
okay		cmp #$80
		bcc okay_2

		and #$0f
		sta scroll_spd

		lda #$00

; Write the byte to the scroller
okay_2		ora #$80
		sta $5fe6

		inc mread+$01
		bne *+$05
		inc mread+$02

		ldx #$00
scr_xb		stx scroll_x
		dey
		bne scroll_loop


; Set up for the next interrupt
		lda #rstr5p
		sta $d012
		lda #$05
		sta rn

		jmp ea31


; Raster split 5
rout5		lda #$7b
		sta $d011
		lda #bg_colour
		sta $d021

		lda scroll_x
		eor #$07
		sta $d016
		lda #$72
		sta $d018

		ldx #$08
		dex
		bne *-$01
		lda #$1b
		sta $d011

; Update the logo positions
		lda #$00
		sta cos_work_2+$01
		sta cos_work_3+$01
		sta cos_work_4+$01

		lda cos_at_2
		clc
		adc #cos_speed_2
		sta cos_at_2
		tax
		lda fli_cosinus,x
		sta cos_work_2+$00

		txa
		clc
		adc #cos_offset_2
		tax
		lda fli_cosinus,x
		sta cos_work_3+$00

		txa
		clc
		adc #cos_offset_2
		tax
		lda fli_cosinus,x
		sta cos_work_4+$00


		lda cos_at_3
		clc
		adc #cos_speed_3
		sta cos_at_3
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_2+$00
		bcc *+$04
		inc cos_work_2+$01
		sta cos_work_2+$00

		txa
		clc
		adc #cos_offset_3
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_3+$00
		bcc *+$04
		inc cos_work_3+$01
		sta cos_work_3+$00

		txa
		clc
		adc #cos_offset_3
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_4+$00
		bcc *+$04
		inc cos_work_4+$01
		sta cos_work_4+$00


		lda cos_at_4
		clc
		adc #cos_speed_4
		sta cos_at_4
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_2+$00
		bcc *+$04
		inc cos_work_2+$01
		sta cos_work_2+$00

		txa
		clc
		adc #cos_offset_4
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_3+$00
		bcc *+$04
		inc cos_work_3+$01
		sta cos_work_3+$00

		txa
		clc
		adc #cos_offset_4
		tax
		lda fli_cosinus,x
		clc
		adc cos_work_4+$00
		bcc *+$04
		inc cos_work_4+$01
		sta cos_work_4+$00

; Decode first row of logo positions
		lda cos_work_2+$00
		and #$07
		eor #$07
		sta logo_d016_m1

		lsr cos_work_2+$01
		ror cos_work_2+$00
		lsr cos_work_2+$01
		ror cos_work_2+$00
		lsr cos_work_2+$01
		ror cos_work_2+$00

		ldy cos_work_2+$00
		lda logo_d018_dcd,y
		sta logo_d018_m1

		lda logo_d023_dcd,y
		sta logo_d021_m1
		lda logo_d021_dcd,y
		sta logo_d022_m1
		lda logo_d022_dcd,y
		sta logo_d023_m1

; Decode second row of logo positions
		lda cos_work_3+$00
		and #$07
		eor #$07
		sta logo_d016_m2

		lsr cos_work_3+$01
		ror cos_work_3+$00
		lsr cos_work_3+$01
		ror cos_work_3+$00
		lsr cos_work_3+$01
		ror cos_work_3+$00

		ldy cos_work_3+$00
		lda logo_d018_dcd,y
		sta logo_d018_m2

		lda logo_d021_dcd,y
		sta logo_d021_m2
		lda logo_d022_dcd,y
		sta logo_d022_m2
		lda logo_d023_dcd,y
		sta logo_d023_m2

; Decode third row of logo positions
		lda cos_work_4+$00
		and #$07
		eor #$07
		sta logo_d016_m3

		lsr cos_work_4+$01
		ror cos_work_4+$00
		lsr cos_work_4+$01
		ror cos_work_4+$00
		lsr cos_work_4+$01
		ror cos_work_4+$00

		ldy cos_work_4+$00
		lda logo_d018_dcd,y
		sta logo_d018_m3

		lda logo_d022_dcd,y
		sta logo_d021_m3
		lda logo_d023_dcd,y
		sta logo_d022_m3
		lda logo_d021_dcd,y
		sta logo_d023_m3


; Set up for the first raster split
		lda #rstr1p
		sta $d012
		lda #$01
		sta rn

; Exit the raster interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti



; Colour data for the FLI effect
		* = ((*/$100)+$01)*$100

colour_table	!byte $09,$09,$02,$09,$02,$02,$08,$02
		!byte $08,$08,$0c,$08,$0c,$0c,$0a,$0c
		!byte $0a,$0a,$0f,$0a,$0f,$0f,$07,$0f
		!byte $07,$07,$01,$07,$01,$01,$0d,$01
		!byte $0d,$0d,$03,$0d,$03,$03,$05,$03
		!byte $05,$05,$0e,$05,$0e,$0e,$04,$0e
		!byte $04,$04,$0b,$04,$0b,$0b,$06,$0b
		!byte $06,$06,$09,$06

		!byte $09,$09,$02,$09,$02,$02,$08,$02
		!byte $08,$08,$0c,$08,$0c,$0c,$0a,$0c
		!byte $0a,$0a,$0f,$0a,$0f,$0f,$07,$0f
		!byte $07,$07,$01,$07,$01,$01,$0d,$01
		!byte $0d,$0d,$03,$0d,$03,$03,$05,$03
		!byte $05,$05,$0e,$05,$0e,$0e,$04,$0e
		!byte $04,$04,$0b,$04,$0b,$0b,$06,$0b
		!byte $06,$06,$09,$06

		!byte $09,$09,$02,$09,$02,$02,$08,$02
		!byte $08,$08,$0c,$08,$0c,$0c,$0a,$0c
		!byte $0a,$0a,$0f,$0a,$0f,$0f,$07,$0f
		!byte $07,$07,$01,$07,$01,$01,$0d,$01
		!byte $0d,$0d,$03,$0d,$03,$03,$05,$03
		!byte $05,$05,$0e,$05,$0e,$0e,$04,$0e
		!byte $04,$04,$0b,$04,$0b,$0b,$06,$0b
		!byte $06,$06

; Cosine table for the FLI effect
		* = ((*/$100)+$01)*$100

fli_cosinus	!byte $93,$93,$93,$93,$93,$93,$93,$92
		!byte $92,$92,$91,$91,$90,$90,$8f,$8f
		!byte $8e,$8d,$8c,$8c,$8b,$8a,$89,$88
		!byte $87,$86,$85,$84,$83,$81,$80,$7f
		!byte $7e,$7c,$7b,$7a,$78,$77,$76,$74
		!byte $73,$71,$6f,$6e,$6c,$6b,$69,$67
		!byte $66,$64,$62,$61,$5f,$5d,$5b,$5a
		!byte $58,$56,$54,$52,$51,$4f,$4d,$4b

		!byte $49,$48,$46,$44,$42,$40,$3e,$3d
		!byte $3b,$39,$37,$36,$34,$32,$30,$2f
		!byte $2d,$2b,$2a,$28,$26,$25,$23,$22
		!byte $20,$1f,$1d,$1c,$1a,$19,$18,$16
		!byte $15,$14,$13,$11,$10,$0f,$0e,$0d
		!byte $0c,$0b,$0a,$09,$08,$07,$07,$06
		!byte $05,$04,$04,$03,$03,$02,$02,$01
		!byte $01,$01,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$01
		!byte $01,$01,$02,$02,$03,$03,$04,$05
		!byte $05,$06,$07,$08,$08,$09,$0a,$0b
		!byte $0c,$0d,$0e,$0f,$11,$12,$13,$14
		!byte $15,$17,$18,$19,$1b,$1c,$1e,$1f
		!byte $21,$22,$24,$25,$27,$29,$2a,$2c
		!byte $2e,$2f,$31,$33,$34,$36,$38,$3a
		!byte $3b,$3d,$3f,$41,$43,$44,$46,$48

		!byte $4a,$4c,$4e,$4f,$51,$53,$55,$57
		!byte $58,$5a,$5c,$5e,$5f,$61,$63,$65
		!byte $66,$68,$6a,$6b,$6d,$6e,$70,$71
		!byte $73,$74,$76,$77,$79,$7a,$7c,$7d
		!byte $7e,$7f,$81,$82,$83,$84,$85,$86
		!byte $87,$88,$89,$8a,$8b,$8c,$8d,$8d
		!byte $8e,$8f,$8f,$90,$90,$91,$91,$92
		!byte $92,$92,$93,$93,$93,$93,$93,$93

; FLI effect preset data
preset_data	!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $04	; speed

		!byte $0d,$0d,$0c,$0c,$0b,$0b,$0a,$0a
		!byte $09,$09,$08,$08,$07,$07,$06,$06
		!byte $05,$05,$04,$04,$03,$03,$02,$02
		!byte $01,$01,$00,$00,$00,$00,$ff,$ff
		!byte $fe,$fe,$fd,$fd,$fc,$fc,$fb,$fb
		!byte $fa,$fa,$f9,$f9,$f8,$f8,$f7,$f7
		!byte $f6,$f6,$f5,$f5,$f4,$f4,$f3,$f3
		!byte $fe	; speed

		!byte $7c,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$7c,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$7c,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $02	; speed

		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $d0,$03,$03,$03,$03,$03,$03,$03
		!byte $fe	; speed

		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $03	; speed

		!byte $02,$02,$02,$02,$02,$02,$02,$02
		!byte $02,$02,$02,$02,$02,$02,$02,$02
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $fc,$fc,$fc,$fc,$fc,$fc,$fc,$fc
		!byte $02,$02,$02,$02,$02,$02,$02,$02
		!byte $02,$02,$02,$02,$02,$02,$02,$02
		!byte $01	; speed

		!byte $ff,$ff,$fe,$fe,$fd,$fd,$fc,$fc
		!byte $fb,$fb,$fa,$fa,$f9,$f9,$f8,$f8
		!byte $f7,$f7,$f6,$f6,$f5,$f5,$f4,$f4
		!byte $f3,$f3,$f2,$f2,$4e,$0e,$0d,$0d
		!byte $0c,$0c,$0b,$0b,$0a,$0a,$09,$09
		!byte $08,$08,$07,$07,$06,$06,$05,$05
		!byte $04,$04,$03,$03,$02,$02,$01,$01
		!byte $03	; speed

		!byte $43,$03,$03,$03,$03,$03,$03,$03
		!byte $bd,$fd,$fd,$fd,$fd,$fd,$fd,$fd
		!byte $43,$03,$03,$03,$03,$03,$03,$03
		!byte $bd,$fd,$fd,$fd,$fd,$fd,$fd,$fd
		!byte $43,$03,$03,$03,$03,$03,$03,$03
		!byte $bd,$fd,$fd,$fd,$fd,$fd,$fd,$fd
		!byte $43,$03,$03,$03,$03,$03,$03,$03
		!byte $02	; speed

		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$fd,$fd,$fd,$fd
		!byte $fd,$fd,$fd,$fd,$fd,$fd,$fd,$fd
		!byte $fd,$fd,$fd,$fd,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$fd,$fd,$fd,$fd
		!byte $fd,$fd,$fd,$fd,$fd,$fd,$fd,$fd
		!byte $01	; speed

		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $fe	; speed

		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $7c,$fc,$fc,$fc,$7c,$fc,$fc,$fc
		!byte $02	; speed

		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $fe	; speed

		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $08,$09,$0a,$0b,$0c,$0d,$0c,$0b
		!byte $0a,$09,$08,$07,$06,$05,$04,$03
		!byte $02,$01,$00,$00,$00,$00,$00,$01
		!byte $02,$03,$04,$05,$06,$07,$08,$09
		!byte $0a,$0b,$0c,$0d,$0c,$0b,$0a,$09
		!byte $08,$07,$06,$05,$04,$03,$02,$01
		!byte $01	; speed

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $07,$07,$07,$07,$07,$07,$07,$07
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $fd	; speed

		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $05,$05,$05,$f4,$f4,$f4,$f4,$f4
		!byte $f4,$f4,$f4,$f4,$f4,$f4,$05,$05
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $05,$05,$f4,$f4,$f4,$f4,$f4,$f4
		!byte $f4,$f4,$f4,$f4,$f4,$05,$05,$05
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $03	; speed

		!byte $80		; end of data marker


; $D018 decoding table for the logos
logo_d018_dcd	!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

		!byte $72,$82,$92,$a2,$b2,$c2,$d2,$e2
		!byte $f2

; Colour decoding tables for the logos
logo_d021_dcd	!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a
logo_d022_dcd	!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $05
logo_d023_dcd	!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		!byte $0e

		!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $05
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		!byte $0e

		!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a
		!byte $05,$05,$05,$05,$05,$05,$05,$05
		!byte $05
		!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
		!byte $0e

; Colours for the scroll line
scroll_cols	!byte $0a,$0a,$08,$08,$02,$02,$09,$09
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$02,$02,$08,$08,$0a,$0a,$0a

; Well it had to appear somewhere...
scroll_text	!scr $83,"welcome to    "

		!scr $81,"--- md201702 ---    "

		!scr $83,"which escaped from the cosine workhouse at "
		!scr "the end of february 2017 along with code and "
		!scr "graphics from t.m.r, another lifted sega master "
		!scr "system character set and excellent music by "
		!scr "odie which was originally used in our c64dtv2 "
		!scr "and c64 game blok copy.    "
		!scr "the first monthly demo back in september 2015 "
		!scr "used an fli-based moving bar effect so here's "
		!scr "another one because they're pretty!"
		!scr "        "

		!scr "despite a nearly two month gap between this and "
		!scr "the previous release at the start of january for "
		!scr "the atari 8-bit, i still haven't got many "
		!scr "bloody ideas for scrolltext...  i'm trying to "
		!scr "steer away from political subjects like brexit "
		!scr "or trump because, apart from having absolutely "
		!scr "nothing positive to say about either, that's "
		!scr "what facebook is for.   that doesn't leave me "
		!scr "with many topics though and i nearly went into "
		!scr "political territory before deciding against it, "
		!scr "but when i run low on other ideas it could "
		!scr "still happen!"
		!scr "        "

		!scr $82,"so with nothing else to say i might as well "
		!scr "get them there greetings out of the way - "
		!scr "cosine make surprisingly rude but otherwise "
		!scr "friendly gestures at..."
		!scr "    "

		!byte $85
		!scr "absence - "
		!scr "abyss connection - "
		!scr "arkanix labs - "
		!scr "artstate - "
		!scr "ate bit - "
		!scr "atlantis - "
		!scr "booze design - "
		!scr "camelot - "
		!scr "censor design - "
		!scr "chorus - "
		!scr "chrome - "
		!scr "cncd - "
		!scr "cpu - "
		!scr "crescent - "
		!scr "crest - "
		!scr "covert bitops - "
		!scr "defence force - "
		!scr "dekadence - "
		!scr "desire - "
		!scr "dac - "
		!scr "dmagic - "
		!scr "dualcrew - "
		!scr "exclusive on - "
		!scr "fairlight - "
		!scr "f4cg - "
		!scr "fire - "
		!scr "flat 3 - "
		!scr "focus - "
		!scr "french touch - "
		!scr "funkscientist productions - "
		!scr "genesis project - "
		!scr "gheymaid inc. - "
		!scr "hitmen - "
		!scr "hokuto force - "
		!scr "legion of doom - "
		!scr "level64 - "
		!scr "maniacs of noise - "
		!scr "mayday - "
		!scr "meanteam - "
		!scr "metalvotze - "
		!scr "noname - "
		!scr "nostalgia - "
		!scr "nuance - "
		!scr "offence - "
		!scr "onslaught - "
		!scr "orb - "
		!scr "oxyron - "
		!scr "padua - "
		!scr "performers - "
		!scr "plush - "
		!scr "professional protection cracking service - "
		!scr "psytronik - "
		!scr "reptilia - "
		!scr "resource - "
		!scr "rgcd - "
		!scr "secure - "
		!scr "shape - "
		!scr "side b - "
		!scr "singular - "
		!scr "slash - "
		!scr "slipstream - "
		!scr "success and trc - "
		!scr "style - "
		!scr "suicyco industries - "
		!scr "taquart - "
		!scr "tempest - "
		!scr "tek - "
		!scr "triad - "
		!scr "trsi - "
		!scr "viruz - "
		!scr "vision - "
		!scr "wow - "
		!scr "wrath "
		!scr "and xenon."
		!scr "        "

		!scr $83,"that lot are usually followed by a short plug "
		!scr "for the cosine website at -"
		!scr $81,"cosine.org.uk -",$83,"and my own blog at -"
		!scr $81,"jasonkelk.me.uk -",$83,"where the occasional "
		!scr "post about what happens "
		!scr $22,"under the hood",$22," with these demos might "
		!scr "appear."
		!scr "        "

		!scr $82,"and this was t.m.r of cosine on 2017-02-28, "
		!scr "wandering off to forage for food... .. .  .   ."
		!scr "            "

		!byte $00		; end of text marker


; Load the nine screens used to animate the logos
		* = $5ca0
		!src "includes/screen_00.asm"

		* = $60a0
		!src "includes/screen_01.asm"

		* = $64a0
		!src "includes/screen_02.asm"

		* = $68a0
		!src "includes/screen_03.asm"

		* = $6ca0
		!src "includes/screen_04.asm"

		* = $70a0
		!src "includes/screen_05.asm"

		* = $74a0
		!src "includes/screen_06.asm"

		* = $78a0
		!src "includes/screen_07.asm"

		* = $7ca0
		!src "includes/screen_08.asm"
