!set block_cnt=$00

!do {

!if block_cnt<>$00 {
		!byte $00,$40,$41,$42,$43,$44,$45,$46
		!byte $47,$00,$80,$81,$82,$83,$84,$85
		!byte $86,$87,$00,$00,$01,$02,$03,$04
		!byte $05,$06,$07,$00,$40,$41,$42,$43
		!byte $44,$45,$46,$47,$00,$80,$81,$82

		!byte $00,$48,$49,$4a,$4b,$4c,$4d,$4e
		!byte $4f,$00,$88,$89,$8a,$8b,$8c,$8d
		!byte $8e,$8f,$00,$08,$09,$0a,$0b,$0c
		!byte $0d,$0e,$0f,$00,$48,$49,$4a,$4b
		!byte $4c,$4d,$4e,$4f,$00,$88,$89,$8a

		!byte $00,$50,$51,$52,$53,$54,$55,$56
		!byte $57,$00,$90,$91,$92,$93,$94,$95
		!byte $96,$97,$00,$10,$11,$12,$13,$14
		!byte $15,$16,$17,$00,$50,$51,$52,$53
		!byte $54,$55,$56,$57,$00,$90,$91,$92
}
		!byte $00,$58,$59,$5a,$5b,$5c,$5d,$5e
		!byte $5f,$00,$98,$99,$9a,$9b,$9c,$9d
		!byte $9e,$9f,$00,$18,$19,$1a,$1b,$1c
		!byte $1d,$1e,$1f,$00,$58,$59,$5a,$5b
		!byte $5c,$5d,$5e,$5f,$00,$98,$99,$9a

		!byte $00,$60,$61,$62,$63,$64,$65,$66
		!byte $67,$00,$a0,$a1,$a2,$a3,$a4,$a5
		!byte $a6,$a7,$00,$20,$21,$22,$23,$24
		!byte $25,$26,$27,$00,$60,$61,$62,$63
		!byte $64,$65,$66,$67,$00,$a0,$a1,$a2

!if block_cnt<>$02 {
		!byte $00,$68,$69,$6a,$6b,$6c,$6d,$6e
		!byte $6f,$00,$a8,$a9,$aa,$ab,$ac,$ad
		!byte $ae,$af,$00,$28,$29,$2a,$2b,$2c
		!byte $2d,$2e,$2f,$00,$68,$69,$6a,$6b
		!byte $6c,$6d,$6e,$6f,$00,$a8,$a9,$aa

		!byte $00,$70,$71,$72,$73,$74,$75,$76
		!byte $77,$00,$b0,$b1,$b2,$b3,$b4,$b5
		!byte $b6,$b7,$00,$30,$31,$32,$33,$34
		!byte $35,$36,$37,$00,$70,$71,$72,$73
		!byte $74,$75,$76,$77,$00,$b0,$b1,$b2

		!byte $00,$78,$79,$7a,$7b,$7c,$7d,$7e
		!byte $7f,$00,$b8,$b9,$ba,$bb,$bc,$bd
		!byte $be,$bf,$00,$38,$39,$3a,$3b,$3c
		!byte $3d,$3e,$3f,$00,$78,$79,$7a,$7b
		!byte $7c,$7d,$7e,$7f,$00,$b8,$b9,$ba

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		}

		!set block_cnt=block_cnt+$01
} until block_cnt=$03
