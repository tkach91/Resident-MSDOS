.286
CSEG segment

    assume cs:CSEG, ds:CSEG, es:CSEG, ss:CSEG
	org 100h
	
Start:

    jmp Init
    Mes_fail db 13, 10, 'Resident was not unloaded!',13, 10, '$'
    ok_mes db 13, 10, 'Resident was unloaded!',13, 10, '$'
	
	Int_09h_proc proc
	
	    pushf
		call dword ptr cs:Int_09h_vect ;������� ������� ������ ����������, 
		                                 ;����� �� �������� ���������� ���������� � ������� ��� � �����
										 ;�.�. ��� ���������� ���������� - ���� ��������� ��� ��������
		pusha
		push ds
		push es
		mov cx, 0040h ;������� � ����� ������ � ��� ��������� ������� ������ BIOS
		push cx
		pop ds
		mov di, word ptr ds:001Ah ;����� ������ ������ ����������
		cmp di, word ptr ds:001Ch ;���� �� ����� ������ ������ => ����� ���� - ������
		je Exit_09h_handler
		
		mov ax, word ptr [di] ;����� ��������� ������
		cmp ah, 42h ;���� ��� �� F8 - �� �����
		jne Exit_09h_handler
		
		mov word ptr ds:001Ch, di ;����� ���������� ����� (��������� ����� ��� ������ � ������ �������)
		
		;��������� �������� ������������� 00
		mov ax, 0500h
		int 10h
		
		call printScr ;��������� ������� ������������� � �����, �������� � �� �������������� �����
		mov byte ptr cs:io_needed, 1 ; ���������� ����, ����������� ������ �� ����
		
		Exit_09h_handler:
		    pop es
			pop ds
			popa
			
			iret
			
	    Int_09h_vect dd ?
	
	Int_09h_proc endp
	
	Int_08h_proc proc
	
	    pushf
		call dword ptr cs:Int_08h_vect ;������� ������� ����������� ����������
		                               ;����� �� �������� ����������, �����
									   ;������ �� ���� ����� ����������
	    pusha
		push ds
		
		cli ;���� ��������� ����������
		
		cmp byte ptr cs:io_needed, 0 ;���������, ����� �� ������ �� ����
		je Exit_08h_handler
		
		call safe_check ;���������, �������� �� ������ �� ����
		jc Exit_08h_handler
		
		mov es, Crit_Seg
		mov ax, es:[Crit_Off]
		cmp ax, 0h
		jne Exit_08h_handler
		
		sti ;��������� ����������
		
		call write_in_file ;��������� ������
		
		Exit_08h_handler:
		    pop ds
			popa
			
			iret
			
	    Int_08h_vect dd ?
	
	Int_08h_proc endp
	
	Int_28h_proc proc
	
        pushf
        push di
        push ds
        push cs
        pop ds
        cli
		
        cmp byte ptr io_needed, 0 ;���������, ����� �� ������ �� ����
        je Go_28h 
        lds di, dword ptr in_dos_addr
        cmp byte ptr [di+1], 1 ;���������, ����� �� ������ �� ����
        ja Go_28h ;(���� ��������� DOS �� ������ ���� ������ 1)
		mov es, Crit_Seg
		mov ax, es:[Crit_Off]
		cmp ax, 0h
		jne Go_28h
		
        sti
		
        call write_in_file ;������ �� ����

	    Go_28h:
		    pop ds
            pop di
            popf
		    jmp dword ptr cs:Int_28h_vect
	
	    Int_28h_vect dd ?
	
	Int_28h_proc endp
	
	;�������� �� ��������� ��������
	Int_67h_proc proc
	
	    pushf
	    cmp ax, 8899h	
        jne Next_test	

        xchg ah, al	
 
        Exit:
		    popf
            jmp dword ptr cs:Int_67h_vect
			
		Next_test:
		    cmp ax, 9999h
			jne Exit
			
			push ds
	        push es
			push dx
			
		    cli
            push 0
            pop ds ;���������� ����� ������� �������� ����������
            mov ax, cs ;��� ���������� �����
            ;���������, �� ����������� �� ����� ��� �����-�� ����������
            cmp ax, word ptr ds:[08h*4+2]
            jne unload_failed				
            cmp ax, word ptr ds:[09h*4+2]
            jne unload_failed			
            cmp ax, word ptr ds:[28h*4+2]
            jne unload_failed				
            cmp ax, word ptr ds:[67h*4+2]
            jne unload_failed	
			
            push cs
		    pop ds
		    mov dx, offset ok_mes
		    mov ah, 09h
		    int 21h
	
            mov ax, 2508h
            lds dx, dword ptr cs:Int_08h_vect
            int 21h		
            mov ax, 2509h
            lds dx, dword ptr cs:Int_09h_vect
            int 21h	
            mov ax, 2528h
            lds dx, dword ptr cs:Int_28h_vect
            int 21h	
            mov ax, 2567h
            lds dx, dword ptr cs:Int_67h_vect
            int 21h	
			
            mov es, word ptr cs:2Ch ;������� ������� ��������� DOS
            mov ah, 49h             ;������� ������������ ������
            int 21h               
		
		    push cs
			pop es
		    mov ah, 49h
		    int 21h
			
			On_Exit:				
			    popf
                pop dx
                pop es
                pop ds
				sti
				jmp dword ptr cs:Int_67h_vect

            unload_failed:
	            push cs
		        pop ds
		        mov dx, offset Mes_fail
		        mov ah, 09h
		        int 21h
                jmp On_Exit				
	
		Int_67h_vect dd ?
	
	Int_67h_proc endp
	
	
	;"������" � ���������� ������
	printScr proc
	
	    push es ;��������� ���������� ��������
        push ds
		
        mov cx, 0B800h
		push cx	;ds ������ ���� ����� �������� ����������� 0-�� ��������
        pop ds

        mov cx, 0B900h
		push cx	;es - 1-�� ��������
        pop es

        xor di, di	;������� di � si
        xor si, si

        mov cx, 25 ;�.�. �� ������ 25 �����
        Next_line:
            push cx
            mov cx, 80 ;80 �������� � ������
            Next_char:
                lodsw ;����� ������ ������ � ������� � AX (lodsW)
                stosb ;������� ������ ��� �������� �� AL � 1-�� ������������� (stosB)
            loop Next_char ;��������� ������ 

            mov ax, 0A0Dh ;��������� � ����� ������ ������ ������� �������
            stosw ;������� ������ 
            pop cx
        loop Next_line	;c�������� ������

		pop ds
		pop es
		
		ret
		
	printScr endp
	
	;������ � ����
	write_in_file proc
	
		pusha
		push cs ;ds ������ ���� ����� ��������, � ������� �� ������ ���������
        pop ds	

        mov byte ptr io_needed, 0 ;�������� ���� ����������� ������ �� ����
		
        mov ah, 3Ch	
        xor cx, cx	
        mov dx, offset Screen_file	
        int 21h	
        push ax ;�������� ����� �����

        mov cx, 0B900h
		push cx	;ds ������ ��������� �� 1-�� �������������
        pop ds
        mov bx, ax ;� bx - ����� �����
        mov ah, 40h	
        xor dx, dx ;ds:dx ������ ��������� �� �����, ��� ��������� ������
        mov cx, 2050 ;���-�� ������������ ���� (80*25 + 2*25 = 2050)
        int 21h	

		;������� ����
        pop bx	
        mov ah, 3Eh	
        int 21h
		
		popa
		ret
		
		Screen_file db 'screen.txt', 0 
	
	write_in_file endp
	
    ;��������� safe_check
    ;���������� CF = 0, ���� � ������ ������ ����� ������������ ��������� DOS,
    ;� CF = 1, ���� ������
    safe_check proc 
	
        push es
        push cs
        pop ds

        les di, dword ptr in_dos_addr ;����� ������ ��������� DOS,
        cmp word ptr es:[di], 0 ; ���� ���� �� ��� �� 0,
        pop es
        jne safe_check_failed ;������������ DOS ������

        clc ;CF = 0
        ret
		
        safe_check_failed:
            stc ;CF = 1
            ret
			
    safe_check endp
	
	in_dos_addr dd ? ;����� ������ ��������� DOS
	Crit_Seg dw 0
	Crit_Off dw 0 ;����� ����������� ������
	io_needed db 0 ;1, ���� ��������� ������ �� ����
	
	Init:
        call Get_cmd  ;�������� ��������� ������

        ;���� ������ � ��������� ������ �� �������, ����� ������� ���������� ��������
        or al, al
        jz Ok_cmd 

        ;���� � ��������� ������ ������� '/u', �� ������� ������� ��������� �� ������
        cmp al, 1
        je Remove  

       ;� ��������� ������ ������� ��������� � �������� ��������� ������ � ����������
       Bad_cmd:
           mov dx, offset Mbad_cmd
           mov ah, 09h
		   int 21h
		   
           ret
		   
		Ok_cmd:
	        ;��������� � ������ �������� ��� ��� ���?
            mov ax, 8899h 
            int 67h    
            cmp ax, 9988h 
            jne Set_resident

            mov ah, 09h ;���� � ������, �� ������� ��������������� ���������
            mov dx, offset In_memory 
            int 21h

            ret	 

        Set_resident:
            ;���� �������� �� � ������ - ��������� ���	
			
			;������� ���� ��������� DOS 
			mov ah, 34h
			int 21h
			dec bx
			
	        mov ah,5Dh 
            int 21h
            mov word ptr Crit_Seg, bx
            mov word ptr Crit_Off, es
			
			mov word ptr in_dos_addr, bx
			mov word ptr in_dos_addr+2, es 
			
			;������� 67h
            mov ax, 3567h
            int 21h
            mov word ptr Int_67h_vect, bx
            mov word ptr Int_67h_vect+2, es	

            mov ax, 2567h
            mov dx, offset Int_67h_proc
            int 21h	

			;����� 09h
			mov ax, 3509h
            int 21h
            mov word ptr Int_09h_vect, bx	
            mov word ptr Int_09h_vect+2, es	

            mov ax, 2509h
            mov dx, offset Int_09h_proc
            int 21h
			
			;������ 28h
			mov ax, 3528h
            int 21h
            mov word ptr Int_28h_vect, bx	
            mov word ptr Int_28h_vect+2, es	

            mov ax, 2528h
            mov dx, offset Int_28h_proc
            int 21h
            		
			;������� 08h
			mov ax, 3508h
            int 21h
            mov word ptr Int_08h_vect, bx	
            mov word ptr Int_08h_vect+2, es	

            mov ax, 2508h
            mov dx, offset Int_08h_proc
            int 21h
			
			mov dx, offset ARight
			mov ah, 09h
			int 21h
			
            mov dx, offset Init
            int 27h 
		
    ;��������� ��������� ������		
	Get_cmd proc
	
		;�������� ��������� ��������� ������
        mov si, 80h ;� si - �������� ��������� ������
        lodsb
        or al, al 
        jz Got_cmd 
        cmp al, 3     
        jne No_string ;�� �����, ���� �������� �� 3 �������

        inc si     

		;�������� ������ � ���������, ������������� �� ��� ������������������ /u
        Next_ch:
            lodsw  
            cmp ax, 'u/' ;������ �������� � �������� �������, ��� ��� ������ ����� ���������
            jne No_string ;�� �����, ���� �� ��� ��������
  
            mov al, 1 ;������ ����, ��� ���� ������� ��������� �� ������
            ret

        Got_cmd:
            xor al, al ;������ ����, ��� ���� �������� ������ ��������� ������
            ret 

        No_string:
            mov al, 3 ;������ ����, ��� � ��������� ������ ��������� �������� ����
            ret ;������� �� ���������
			
    Get_cmd endp
	
	Remove:
        mov ax, 8899h 
        int 67h 
        cmp ax, 9988h
        je Remove_res	
		
		mov dx, offset Not_in_mem
		mov ah, 09h
		int 21h 
		int 20h

    Remove_res:		
	    mov ax, 9999h
		int 67h

        int 20h
        			
    In_memory db 13, 10, 'Resident already in memory!', 13, 10, '$'
	ARight db 13, 10, 'Resident was sucksessfully installed!', 13, 10, 'F8 - save screen contents in file ', 13, 10 
	       db         '<resident name> /u in command line - unload resident', 13, 10, '$'
	Mbad_cmd db 13, 10, 'Incorrect input in command prompt!', 13, 10, '$'
	Not_in_mem db 13, 10, 'There was not resident in memory!', 13, 10, '$'
            							
CSEG ends
end Start