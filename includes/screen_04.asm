!set block_cnt=$00

!do {

!if block_cnt<>$00 {
		!byte $04,$05,$06,$07,$00,$40,$41,$42
		!byte $43,$44,$45,$46,$47,$00,$80,$81
		!byte $82,$83,$84,$85,$86,$87,$00,$00
		!byte $01,$02,$03,$04,$05,$06,$07,$00
		!byte $40,$41,$42,$43,$44,$45,$46,$47

		!byte $0c,$0d,$0e,$0f,$00,$48,$49,$4a
		!byte $4b,$4c,$4d,$4e,$4f,$00,$88,$89
		!byte $8a,$8b,$8c,$8d,$8e,$8f,$00,$08
		!byte $09,$0a,$0b,$0c,$0d,$0e,$0f,$00
		!byte $48,$49,$4a,$4b,$4c,$4d,$4e,$4f

		!byte $14,$15,$16,$17,$00,$50,$51,$52
		!byte $53,$54,$55,$56,$57,$00,$90,$91
		!byte $92,$93,$94,$95,$96,$97,$00,$10
		!byte $11,$12,$13,$14,$15,$16,$17,$00
		!byte $50,$51,$52,$53,$54,$55,$56,$57
}
		!byte $1c,$1d,$1e,$1f,$00,$58,$59,$5a
		!byte $5b,$5c,$5d,$5e,$5f,$00,$98,$99
		!byte $9a,$9b,$9c,$9d,$9e,$9f,$00,$18
		!byte $19,$1a,$1b,$1c,$1d,$1e,$1f,$00
		!byte $58,$59,$5a,$5b,$5c,$5d,$5e,$5f

		!byte $24,$25,$26,$27,$00,$60,$61,$62
		!byte $63,$64,$65,$66,$67,$00,$a0,$a1
		!byte $a2,$a3,$a4,$a5,$a6,$a7,$00,$20
		!byte $21,$22,$23,$24,$25,$26,$27,$00
		!byte $60,$61,$62,$63,$64,$65,$66,$67

!if block_cnt<>$02 {
		!byte $2c,$2d,$2e,$2f,$00,$68,$69,$6a
		!byte $6b,$6c,$6d,$6e,$6f,$00,$a8,$a9
		!byte $aa,$ab,$ac,$ad,$ae,$af,$00,$28
		!byte $29,$2a,$2b,$2c,$2d,$2e,$2f,$00
		!byte $68,$69,$6a,$6b,$6c,$6d,$6e,$6f

		!byte $34,$35,$36,$37,$00,$70,$71,$72
		!byte $73,$74,$75,$76,$77,$00,$b0,$b1
		!byte $b2,$b3,$b4,$b5,$b6,$b7,$00,$30
		!byte $31,$32,$33,$34,$35,$36,$37,$00
		!byte $70,$71,$72,$73,$74,$75,$76,$77

		!byte $3c,$3d,$3e,$3f,$00,$78,$79,$7a
		!byte $7b,$7c,$7d,$7e,$7f,$00,$b8,$b9
		!byte $ba,$bb,$bc,$bd,$be,$bf,$00,$38
		!byte $39,$3a,$3b,$3c,$3d,$3e,$3f,$00
		!byte $78,$79,$7a,$7b,$7c,$7d,$7e,$7f

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		}

		!set block_cnt=block_cnt+$01
} until block_cnt=$03
