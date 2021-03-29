[org 0x0100]
	jmp Start
	message1: db 'Enter Your Name:',0
	Names: times 12 db 0,0
;---------------------------------------------------------------------------------------------------
Delay:
	push ax
	mov ax, 0xffff
	delayloop:
	sub ax, 1
	jnz delayloop
	
	pop ax
	ret
	
Delay2:
	push ax
	mov ax, 0x0fff
	delayloop2:
	sub ax, 1
	jnz delayloop2
	
	pop ax
	ret
;---------------------------------------------------------------------------------------------
clrscr:			;clear screen subroutine
	push si
	mov si, 0xb800
	mov es, si
	mov si, 0
clrloop:
	mov word [es:si], 0x0000
	add si, 2
	cmp si, 4000
	jne clrloop
	pop si 
	ret
	
clrdash:
	push si
	push ax
	mov si, 0xb800
	mov es, si
	mov si, 0
	mov ah, 7
	mov al, '-'
dashloop:
	call Delay2
	mov [es:si], ax
	add si, 2
	cmp si, 4000
	jne dashloop
	pop ax
	pop si 
	ret


;-------------------------------------------------------------------------------------------------
printstr: 
	 push bp
	 mov bp, sp
	 push es
	 push ax
	 push cx
	 push si
	 push di
		 mov ax, 0xb800
		 mov es, ax ; point es to video base
		 mov al, 80 ; load al with columns per row
		 mul byte [bp+10] ; multiply with y position
		 add ax, [bp+12] ; add x position
		 shl ax, 1 ; turn into byte offset
		 mov di, ax ; point di to required location
		 mov si, [bp+6] ; point si to string
		 mov cx, [bp+4] ; load length of string in cx
		 mov ah, [bp+8] ; load attribute in ah
		nextchar: mov al, [si] ; load next char of string
		 mov [es:di], ax ; show this char on screen
		 add di, 2 ; move to next screen location
		 add si, 1 ; move to next char in string
		 loop nextchar ; repeat the operation cx times
	 pop di
	 pop si
	 pop cx
	 pop ax
	 pop es
	 pop bp
	 ret 10
	 
	 
;------------------------------------------------GENERAL FUNCTION-------------------------------------------------------
Lose:
	push ax
	push bx
	push cx
	push dx
			mov ax, [xen]
			cmp word [cs:xpos], ax
			je checkll3
			;jmp exitlose
			checkll2:	
					add ax, 3
					cmp word [cs:xpos], ax
					je checkll3
					jmp exitlose
	checkll3:
	call clrdash
	mov byte [ds:endg], 1
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x40 ; normal attrib
	mov dx, 0x0A20 ; 
	mov cx, 10 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message5 ; offset of string
	int 0x10 ; call BIOS video service
	
	mov ah, 0x40 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x0A ; normal attrib
	mov dx, 0x0B10 ; 
	mov cx, 12 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message4 ; offset of string
	int 0x10 ; call BIOS video service
	
	mov ah, 0x40 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x0A ; normal attrib
	mov dx, 0x0B20 ; 
	mov cx, 12 ; length of string
	push cs
	pop es ; segment of string
	mov bp, Names ; offset of string
	int 0x10 ; call BIOS video service

	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop es
	pop ax
	mov ax, 0x4c00 ; terminate and stay resident
	int 0x21 
	exitlose:
	pop dx
	pop cx
	pop bx 
	pop ax
	ret
	


Win:
	push ax
	push bx
	push cx
	push dx
			cmp word [cs:xpos], 66
			jge checkl2
			jmp exitwin
			checkl2:	
					cmp word [cs:xpos], 69
					jle checkl3
					jmp exitwin
	checkl3:
	call clrdash
	mov byte [ds:endg], 1
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x8A ; normal attrib
	mov dx, 0x0A20 ; 
	mov cx, 10 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message ; offset of string
	int 0x10 ; call BIOS video service
	
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x0A ; normal attrib
	mov dx, 0x0B10 ; 
	mov cx, 12 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message4 ; offset of string
	int 0x10 ; call BIOS video service
	
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x0A ; normal attrib
	mov dx, 0x0B20 ; 
	mov cx, 12 ; length of string
	push cs
	pop es ; segment of string
	mov bp, Names ; offset of string
	int 0x10 ; call BIOS video service

	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop es
	pop ax
	mov ax, 0x4c00 ; terminate and stay resident
	int 0x21 
	exitwin:
	pop dx
	pop cx
	pop bx 
	pop ax
	ret
;===============================================================================================================
PrintScreen1:
	call clrdash
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0x04 
	mov dx, 0x0B20 ; 
	mov cx, 16 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message3 ; offset of string
	int 0x10 ; call BIOS video service
	
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, 0xb0 
	mov dx, 0x1705 ; 
	mov cx, 25 ; length of string
	push cs
	pop es ; segment of string
	mov bp, message2 ; offset of string
	int 0x10 ; call BIOS video service
	
	
		mov ah, 0x10 ; service 10 – vga attributes
		mov al, 03 ; subservice 3 – toggle blinking
		mov bl, 01 ; enable blinking bit
		int 0x10 ; call BIOS video service
		mov ah, 0 ; service 0 – get keystroke
		int 0x16 ; call BIOS keyboard service
	ret
;------------------------------------------------GENERAL FUNCTION-------------------------------------------------------	
Box:
	push bp
	mov bp, sp
	push si
	push di
	push bx
	push ax
	push es
	mov si, 0xb800
	mov es, si
	
	mov al, 80		;algorithm for x and y position
	mul byte [bp+4]
	add ax, [bp+6]
	shl ax, 1
	mov bx, ax
	
	mov ah, [bp+12]	;byte attribute
	mov al, 0x20	;ASCII value
	mov si, 0		;for length
	mov di, 0		;for breadth 
	
	yline:			;nested loop for rectangle
		mov di, 0
		xline:
			mov [es:bx], ax
			add bx, 2
			add di, 2
			cmp di, [bp+8]
			jbe xline
		
		sub bx, di
		add bx, 160
		inc si
		cmp si, [bp+10]
		jne yline
		
	pop es	
	pop ax
	pop bx
	pop di
	pop si
	pop bp
	ret 10
	
;------------------------------------------------PRINT HURDLE-------------------------------------------------------	
Hurdle:
	push  bp
	mov bp, sp
	push ax
	push es
	
	mov ax, 0x40	;attribute for hurdle
	push ax
	mov ax, [bp+8]		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	push ax
	mov ax, [bp+4]		;y-position
	push ax
	call Box
	
	mov ax, 0x40	;attribute for hurdle
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	sub ax, 2
	push ax
	mov ax, [bp+4]		;y-position
	push ax
	call Box
	
	mov ax, 0x40	;attribute for hurdle
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 2
	push ax
	mov ax, [bp+4]		;y-position
	push ax
	call Box
	
	pop es
	pop ax
	pop bp
	ret 6
;--------------------------------------------Kingdom----------------------------------------------------------
Kingdom:
	push  bp
	mov bp, sp
	push ax
	push es
	
	mov ax, 0x77		;attribute for hurdle
	push ax
	mov ax, [bp+8]		;length
	push ax
	mov ax, 10		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	push ax
	mov ax, [bp+4]		;y-position
	push ax
	call Box

	pop es
	pop ax
	pop bp
	ret 6

;--------------------------------------------------------------
kWrap:
	push ax
	push es
	push di
	
	push 0xb800
	pop es
	
	mov ax, 15			;length
	push ax
	mov ax, 12			;x position 
	push ax
	mov ax, 10			;y position
	push ax
	call Kingdom
	
	mov ax, 15			;length
	push ax
	mov ax, 54			;x position 
	push ax
	mov ax, 10			;y position
	push ax
	call Kingdom


	mov ax, 15			;length
	push ax
	mov ax, 33			;x position 
	push ax
	mov ax, 10			;y position
	push ax
	call Kingdom
	
	mov di,1464
	l0l:
	mov word [es:di],0x7700
	add di,2
	cmp di,1560
	jne l0l
	
	mov di,1314
	l1l:
	mov word [es:di],0x7700
	add di,2
	cmp di,1390
	jne l1l
	
	mov di,1164
	l2l:
	mov word [es:di],0x7700
	add di,2
	cmp di,1220
	jne l2l
	
	mov di,1014
	l3l:
	mov word [es:di],0x7700
	add di,2
	cmp di,1050
	jne l3l
	
	mov di,864
	l4l:
	mov word [es:di],0x7700
	add di,2
	cmp di,880
	jne l4l

	mov di,708
	l5l:
	mov word [es:di],0x7700
	add di,2
	cmp di,716
	jne l5l

	pop di
	pop es
	pop ax
	ret
	
	
;-------------------------------------------PRINT MARIO-------------------------------------------------------------------------------
Mario:
	push bp
	mov bp, sp
	push ax
	push bx
	push es
	
	;-------Body-----------------------
	mov ax, 0x30	;attribute for body
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 1
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	;----------Legs--------------------
	mov ax, 0x60	;attribute for legs
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 1
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 2
	push ax
	call Box
	
	mov ax, 0x60	;attribute for legs
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 3
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 2
	push ax
	call Box
	
	;-----------------Hands-----------------
	mov ax, 0x60	;attribute for hands
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	
	mov ax, 0x60	;attribute for hands
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 4
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	
	;-----------------Neck-------------------
	mov ax, 0x60	;attribute for neck
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 2
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 5
	push ax
	call Box
	
	;--------------face-------------------------
	mov ax, 0xb800
	mov es, ax
	
	mov al, 80		;algorithm for x and y position
	mov bl, [bp+4]
	sub bl, 6
	mul bl
	add ax, [bp+6]
	add ax, 1
	shl ax, 1
	mov bx, ax
	

	mov ah, 0x30
	mov al, '~'
	mov [es:bx], ax
	add bx, 2
	mov al, '-'
	mov [es:bx], ax
	add bx, 2
	mov al, '~'
	mov [es:bx], ax
	
	pop es
	pop bx
	pop ax
	pop bp
	ret 4
;---------------------------------------CLEAR MARIO --------------------------------------------
ClrMario:
	push bp
	mov bp, sp
	push ax
	push bx
	push es
	
	;-------Body-----------------------
	mov ax, 0x07	;attribute for body
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 1
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	;----------Legs--------------------
	mov ax, 0x07	;attribute for legs
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 1
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 2
	push ax
	call Box
	
	mov ax, 0x07	;attribute for legs
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 3
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 2
	push ax
	call Box
	
	;-----------------Hands-----------------
	mov ax, 0x07	;attribute for hands
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	
	mov ax, 0x07	;attribute for hands
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 4
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 4
	push ax
	call Box
	
	;-----------------Neck-------------------
	mov ax, 0x07	;attribute for neck
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [bp+6]		;x-position
	add ax, 2
	push ax
	mov ax, [bp+4]		;y-position
	sub ax, 5
	push ax
	call Box
	
	;--------------face-------------------------
	mov ax, 0xb800
	mov es, ax
	
	mov al, 80		;algorithm for x and y position
	mov bl, [bp+4]
	sub bl, 6
	mul bl
	add ax, [bp+6]
	add ax, 1
	shl ax, 1
	mov bx, ax
	

	mov ah, 0x07
	mov al, ' '
	mov [es:bx], ax
	add bx, 2
	mov al, ' '
	mov [es:bx], ax
	add bx, 2
	mov al, ' '
	mov [es:bx], ax
	
	call kWrap
	call PrintBackgrround
	pop es
	pop bx
	pop ax
	pop bp
	ret 4
;----------------------------------------------------------------------------------------------
Monster:
	push ax
	push cx
	push si
	push es

	mov ax, 0xb800
	mov es, ax
	
	mov ax, 0x40	;attribute for body
	push ax
	mov ax, 2		;length
	push ax
	mov ax, 10		;breadth
	push ax
	mov ax, [xmon];x-position
	push ax
	mov ax, 0		;y-position
	push ax
	call Box

	mov ax, 320
	add ax, [xmon]
	add ax, [xmon]
	mov cx, 10
	mov si, ax
	mov ah, 0x40
	mov al, 'v'
	printmons:
		mov [es:si], ax
		loop printmons
	
	pop es
	pop si
	pop cx
	pop ax
ret
	
	
;=============================================================================================
Enemy:	
	push ax
	mov ax, [xen]
	cmp ax, 58
	je en2
	jl en1
	cmp ax, 45
	je en1
	;jl en2
	
	en1: add word [cs:xen], 1
	jmp ens
	en2: sub word [cs:xen], 1
	
	ens:
	mov ax, 0x50	;attribute for body
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [xen];x-position
	push ax
	mov ax, 24		;y-position
	push ax
	call Box
	
	mov ax, 0x50	;attribute for body
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [xen]		;x-position
	add ax, 1
	push ax
	mov ax, 23		;y-position
	push ax
	call Box
	pop ax
ret
	
	
	clrEnemy:
	push ax
	mov ax, 0x07	;attribute for body
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 4		;breadth
	push ax
	mov ax, [xen];x-position
	push ax
	mov ax, 24		;y-position
	push ax
	call Box
	
	mov ax, 0x07	;attribute for body
	push ax
	mov ax, 1		;length
	push ax
	mov ax, 1		;breadth
	push ax
	mov ax, [xen]		;x-position
	add ax, 1
	push ax
	mov ax, 23		;y-position
	push ax
	call Box
	pop ax
ret
	
	
	
	
;-------------------------------------------GAME FLAG-----------------------------------------------------------	
GameFlag:
	push ax
	push bx
	push es
	
	mov ax, 0x20	;attribute for Flag
	push ax
	mov ax, 5		;length
	push ax
	mov ax, 18		;breadth
	push ax
	mov ax, 70		;x-position
	push ax
	mov ax, 0		;y-position
	push ax
	call Box
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 76
	shl ax, 1
	mov bx, ax
	
; PrintStar
	mov ah, 0x01
	mov al, '*'
	mov [es:bx], ax 
	
	add bx, 156
	mov [es:bx], ax 
	add bx, 4
	mov [es:bx], ax 
	add bx, 4
	mov [es:bx], ax 
	
	sub bx, 6
	mov al, ' '
	mov [es:bx], ax
	add bx, 4
	mov [es:bx], ax
	
	
	
	mov al, '*'
	add bx, 156
	mov [es:bx], ax 
	add bx, 4
	mov [es:bx], ax 
	
	pop es
	pop bx
	pop ax
	ret
;--------------------------------------------------------------------------------------------------------	
PrintBackgrround:
	
	;call Monster
	push ax
	;--------Hurdle # 1----------------------
	mov ax, 5		;length
	push ax
	mov ax, 20			;x position 
	push ax
	mov ax, 21			;y position
	push ax
	call Hurdle
	
	;------------Hurdle # 2-------------------- 
	mov ax, 8			;length
	push ax
	mov ax, 40			;x position 
	push ax
	mov ax, 19			;y position
	push ax
	call Hurdle
	
	;-------------------Hurdle # 3-----------------------
	mov ax, 11			;length
	push ax
	mov ax, 65			;x position 
	push ax
	mov ax, 15			;y position
	push ax
	call Hurdle
	
	;Print Flag
	call GameFlag
	
	pop ax
	ret
	
;=============================================================================================================================

kbisr: 
push ax
push es

	in al, 0x60 ; read a char from keyboard port
	cmp al, 0x4d ; has the right key
	jne nextcmp ; no, try next comparison
		mov ax, [cs:xpos]
		cmp ax, 75			;check if corner of screen is reached
		je ll
		
		cmp ax, 13			;check hurdle 1
		je ll
		cmp ax, 33			;check hurdle 2
		je ll
		cmp ax, 58			;check hurdle 3
		je ll
		
		mov ax, [cs:xpos]
		cmp ax, 24			;check if end of hurdle1 reached
		je downr
		cmp ax, 44			;check if end of hurdle2 reached
		je downr
		cmp ax, 69			;check if end of hurdle3 reached
		je downr
		jmp normr
				downr:		;if end of any of the given hurdles reached, the character will go down on the ground
				push word [cs:xpos]
				push word [cs:ypos]
				call ClrMario
				mov word [cs:ypos], 25; 
				push word [cs:xpos]
				push word [cs:ypos]
				call Mario
				;jmp ll
		
	normr:					;normal movement through right key
		push word [cs:xpos]
		push word [cs:ypos]
		call ClrMario
		add word [cs:xpos], 1; 
		push word [cs:xpos]
		push word [cs:ypos]
		call Mario
		call PrintBackgrround
	ll: jmp exit ; leave interrupt routine




	nextcmp: cmp al, 0x4b ; has the left key 
	jne nextcmp2 ; no, try next comparison
		mov ax, [cs:xpos]
		cmp ax, 0			;check if corner of screen reached
		je lr
		cmp ax, 25			;check hurdle 1
		je lr
		cmp ax, 45			;check hurdle 2
		je lr
		cmp ax, 70			;check hurdle 3
		je lr
		
		mov ax, [cs:xpos]
		cmp ax, 14			;check if corner of hurdle1 reached
		je downl
		cmp ax, 34			;check if corner of hurdle2 reached
		je downl
		cmp ax, 59			;check if corner of hurdle3 reached
		je downl
		jmp norml
				downl:		;if end of any of the given hurdles reached, the character will go down on the ground
				push word [cs:xpos]
				push word [cs:ypos]
				call ClrMario
				mov word [cs:ypos], 25; 
				push word [cs:xpos]
				push word [cs:ypos]
				call Mario
				;jmp ll
		
	norml:		;normal movement through left key
		
		push word [cs:xpos]
		push word [cs:ypos]
		call ClrMario
		sub word [cs:xpos], 1; 
		push word [cs:xpos]
		push word [cs:ypos]
		call Mario
		call PrintBackgrround
	lr:jmp nextcmp3e ; leave interrupt routine



	nextcmp2:
	cmp al, 0x48 ; has the up key pressed
	jne nextcmp3e ; no, try next comparison
		mov ax, [cs:xpos]
		cmp ax, 13		;check if corner of hurdle1 reached
		je rup1e
		cmp ax, 33		;check if corner of hurdle2 reached
		je rup2e
		cmp ax, 58		;check if corner of hurdle3 reached
		je rup3e
		
		mov ax, [cs:xpos]
		cmp ax, 25		;check if corner of hurdle1 reached
		je lup1ee
		cmp ax, 45		;check if corner of hurdle2 reached
		je lup2ee
		cmp ax, 70		;check if corner of hurdle3 reached
		je lup3ee
		
		push word [cs:xpos]		;normal movement through up key
		push word [cs:ypos]
		call ClrMario
		sub word [cs:ypos], 9; 
		push word [cs:xpos]
		push word [cs:ypos]
		call Mario
		
		call Delay
		call Delay
		call Delay
		call Delay
		push word [cs:xpos]
		push word [cs:ypos]
		call ClrMario
		add word [cs:ypos], 9; 
		push word [cs:xpos]
		push word [cs:ypos]
		call Mario
		call GameFlag
		call Win
		jmp exit ; 
	
	nextcmp3e: jmp nextcmp3
	rup1e: jmp rup1
	rup2e: jmp rup2
	rup3e: jmp rup3	
	lup1ee:jmp lup1e
	lup2ee:jmp lup2e
	lup3ee:jmp lup3e
	
		rup1:					;algo for corner of hurdle1 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			add word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			add word [cs:ypos], 5; 
			add word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e
		rup2:					;algo for corner of hurdle2 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			add word [cs:ypos], 3; 
			add word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e
		rup3:					;algo for corner of hurdle3 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 1; 
			add word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e
			
		lup1e: jmp lup1
		lup2e: jmp lup2
		lup3e: jmp lup3
			lup1:					;algo for corner of hurdle1 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			sub word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			add word [cs:ypos], 5; 
			sub word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e
		lup2:					;algo for corner of hurdle2 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			add word [cs:ypos], 3; 
			sub word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e
		lup3:					;algo for corner of hurdle3 reached
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 9; 
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			
			call Delay
			call Delay
			call Delay
			call Delay
			push word [cs:xpos]
			push word [cs:ypos]
			call ClrMario
			sub word [cs:ypos], 1; 
			sub word [cs:xpos], 1
			push word [cs:xpos]
			push word [cs:ypos]
			call Mario
			jmp nextcmp3e

	nextcmp3: 
		nomatch: pop es
		 pop ax
		 jmp far [cs:oldisr] ; call the original ISR

exit: mov al, 0x20
 out 0x20, al ; send EOI to PIC
 pop es
 pop ax
 iret ; return from interrupt
 
 ;================================================================================================
timer: push ax
		push dx
		push cx
	mov dx, 0
	mov cx, 60
	add word [timerv],1
	mov ax, [timerv]
	div cx
	cmp dx, 0
	je enp
	jmp exittime
	enp: 
		call clrEnemy
		call Enemy
	
exittime
 mov al, 0x20
 out 0x20, al ; send EOI to PIC
pop cx
pop dx
 pop ax
 iret  
	

;========================================================================================================
PrintString:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	
	mov ah, 0x13 ; service 13 - print string
	mov al, 1 ; subservice 01 – update cursor
	mov bh, 0 ; output on page 0
	mov bl, [bp+10] ; normal attrib
	mov dx, [bp+8] ; row 10 column 3
	mov cx, [bp+6] ; length of string
	push cs
	pop es ; segment of string
	mov bp, [bp+4] ; offset of string
	int 0x10 ; call BIOS video service
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop bp
	ret 4
	
;=================================================================================================
GetName:
	push ax
	push bx
	push di
	push es
	
	push 0xb800
	pop es
	mov bx,0
	mov di,674
	loop1:
		mov ah,0				; Get Keystroke - Subservice 0
		int 0x16				; BIOS keyboard service
		cmp al, 0x8
		je here
		jne there
		here:
			sub di,2
			mov word [es:di],0x0000
			dec bx
			mov byte [Names+bx],''
			jmp again
			
		there:
			cmp al,0xD				; Compare to Enter key
			je endl					; If enter is hit, terminate the program
			mov [Names+bx], al
			
			print:
				mov ah,07				; set attributes
				mov word [es:di],ax		; Display the key pressed on screen
				add di,2				; increment cursor location	
				inc bx
				
				again:
					cmp bx,14
					jne loop1
			
	endl:
	pop es
	pop di 
	pop bx
	pop ax
	ret
	
;=================================================================================================	
Start:
	call PrintScreen1
	call clrscr
	
	push 7
	push 0x0400
	push 16
	push message1
	call PrintString

	call GetName
	call clrscr
	
	call kWrap
	call PrintBackgrround
	
	push 71
	push 0x0000
	push 12
	push Names
	call PrintString
	
	;-----------------Print Character---------------------
	mov ax, [xpos]			;x position 
	push ax
	mov ax, [ypos]			;y position
	push ax
	call Mario
	call Enemy

 
	xor ax, ax
	mov es, ax ; point es to IVT base
	mov ax, [es:9*4]
	mov [oldisr], ax ; save offset of old routine
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax ; save segment of old routine
	
	cli ; disable interrupts
	mov word [es:9*4], kbisr ; store offset at n*4
	mov [es:9*4+2], cs ; store segment at n*4+2
	sti ; enable interrupts
	cmp byte [endg], 0
	jnz unhook
	
	mov dx, Start ; end of resident portion
	add dx, 15 ; round up to next para
	mov cl, 4
	shr dx, cl ; number of paras
	mov ax, 0x3100 ; terminate and stay resident
	int 0x21 

;-----------unhooking-------------------
unhook:
	mov ax, [oldisr]
	mov [es:9*4], ax
	mov ax, [oldisr+2]
	mov [es:9*4+2], ax
	mov ax, 0x4c00
	int 0x21
	
;starting positions of character, from left most bottom part of screen
xpos: dw 0
ypos: dw 25
oldisr: dd 0
message: db 'YOU WIN!!!'
message2: db 'Press any key to continue!'
message3: db 'Super Mario Mini'
message4: db 'Player Name:'
message5: db 'YOU LOSE!!'
xen: dw 45
timerv: dw 0
xmon: dw 15
endg: db 0
;message1: db 'Enter Your Name:',0
;	Names: times 12 db 0,0


;Starting points of hurdles in respective order: 13		33		58
;Starting points of hurdles in respective order: 25		45		70