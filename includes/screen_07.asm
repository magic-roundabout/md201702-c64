!set block_cnt=$00

!do {

!if block_cnt<>$00 {
		!byte $07,$00,$40,$41,$42,$43,$44,$45
		!byte $46,$47,$00,$80,$81,$82,$83,$84
		!byte $85,$86,$87,$00,$00,$01,$02,$03
		!byte $04,$05,$06,$07,$00,$40,$41,$42
		!byte $43,$44,$45,$46,$47,$00,$80,$81

		!byte $0f,$00,$48,$49,$4a,$4b,$4c,$4d
		!byte $4e,$4f,$00,$88,$89,$8a,$8b,$8c
		!byte $8d,$8e,$8f,$00,$08,$09,$0a,$0b
		!byte $0c,$0d,$0e,$0f,$00,$48,$49,$4a
		!byte $4b,$4c,$4d,$4e,$4f,$00,$88,$89

		!byte $17,$00,$50,$51,$52,$53,$54,$55
		!byte $56,$57,$00,$90,$91,$92,$93,$94
		!byte $95,$96,$97,$00,$10,$11,$12,$13
		!byte $14,$15,$16,$17,$00,$50,$51,$52
		!byte $53,$54,$55,$56,$57,$00,$90,$91
}
		!byte $1f,$00,$58,$59,$5a,$5b,$5c,$5d
		!byte $5e,$5f,$00,$98,$99,$9a,$9b,$9c
		!byte $9d,$9e,$9f,$00,$18,$19,$1a,$1b
		!byte $1c,$1d,$1e,$1f,$00,$58,$59,$5a
		!byte $5b,$5c,$5d,$5e,$5f,$00,$98,$99

		!byte $27,$00,$60,$61,$62,$63,$64,$65
		!byte $66,$67,$00,$a0,$a1,$a2,$a3,$a4
		!byte $a5,$a6,$a7,$00,$20,$21,$22,$23
		!byte $24,$25,$26,$27,$00,$60,$61,$62
		!byte $63,$64,$65,$66,$67,$00,$a0,$a1

!if block_cnt<>$02 {
		!byte $2f,$00,$68,$69,$6a,$6b,$6c,$6d
		!byte $6e,$6f,$00,$a8,$a9,$aa,$ab,$ac
		!byte $ad,$ae,$af,$00,$28,$29,$2a,$2b
		!byte $2c,$2d,$2e,$2f,$00,$68,$69,$6a
		!byte $6b,$6c,$6d,$6e,$6f,$00,$a8,$a9

		!byte $37,$00,$70,$71,$72,$73,$74,$75
		!byte $76,$77,$00,$b0,$b1,$b2,$b3,$b4
		!byte $b5,$b6,$b7,$00,$30,$31,$32,$33
		!byte $34,$35,$36,$37,$00,$70,$71,$72
		!byte $73,$74,$75,$76,$77,$00,$b0,$b1

		!byte $3f,$00,$78,$79,$7a,$7b,$7c,$7d
		!byte $7e,$7f,$00,$b8,$b9,$ba,$bb,$bc
		!byte $bd,$be,$bf,$00,$38,$39,$3a,$3b
		!byte $3c,$3d,$3e,$3f,$00,$78,$79,$7a
		!byte $7b,$7c,$7d,$7e,$7f,$00,$b8,$b9

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		}

		!set block_cnt=block_cnt+$01
} until block_cnt=$03
