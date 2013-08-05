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
		call dword ptr cs:Int_09h_vect ;сначала вызвали старый обработчик, 
		                                 ;чтобы он завершил аппаратное прерывание и передал код в буфер
										 ;т.к. это аппаратное прерывание - надо сохранить все регистры
		pusha
		push ds
		push es
		mov cx, 0040h ;начиная с этого адреса в ОЗУ находится область данных BIOS
		push cx
		pop ds
		mov di, word ptr ds:001Ah ;адрес головы буфера клавиатуры
		cmp di, word ptr ds:001Ch ;если он равен адресу хвоста => буфер пуст - уходим
		je Exit_09h_handler
		
		mov ax, word ptr [di] ;иначе считываем символ
		cmp ah, 42h ;если это не F8 - на выход
		jne Exit_09h_handler
		
		mov word ptr ds:001Ch, di ;иначе опустошаем буфер (установим адрес его головы и хвоста равными)
		
		;Установим активной видеостраницу 00
		mov ax, 0500h
		int 10h
		
		call printScr ;сохранили текущую видеостраницу в буфер, почистив её от дополнительных кодов
		mov byte ptr cs:io_needed, 1 ; установили флаг, требующейся записи на диск
		
		Exit_09h_handler:
		    pop es
			pop ds
			popa
			
			iret
			
	    Int_09h_vect dd ?
	
	Int_09h_proc endp
	
	Int_08h_proc proc
	
	    pushf
		call dword ptr cs:Int_08h_vect ;сначала вызвали стандартный обработчик
		                               ;чтобы он завершил прерывание, иначе
									   ;запись на диск будет невозможна
	    pusha
		push ds
		
		cli ;пока запрещаем прерывания
		
		cmp byte ptr cs:io_needed, 0 ;проверяем, нужна ли запись на диск
		je Exit_08h_handler
		
		call safe_check ;проверяем, возможна ли запись на диск
		jc Exit_08h_handler
		
		mov es, Crit_Seg
		mov ax, es:[Crit_Off]
		cmp ax, 0h
		jne Exit_08h_handler
		
		sti ;разрешили прерывания
		
		call write_in_file ;произвели запись
		
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
		
        cmp byte ptr io_needed, 0 ;проверить, нужно ли писать на диск
        je Go_28h 
        lds di, dword ptr in_dos_addr
        cmp byte ptr [di+1], 1 ;проверить, можно ли писать на диск
        ja Go_28h ;(флаг занятости DOS не должен быть больше 1)
		mov es, Crit_Seg
		mov ax, es:[Crit_Off]
		cmp ax, 0h
		jne Go_28h
		
        sti
		
        call write_in_file ;запись на диск

	    Go_28h:
		    pop ds
            pop di
            popf
		    jmp dword ptr cs:Int_28h_vect
	
	    Int_28h_vect dd ?
	
	Int_28h_proc endp
	
	;Проверка на повторную загрузку
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
            pop ds ;сегментный адрес таблицы векторов прерываний
            mov ax, cs ;наш сегментный адрес
            ;проверяем, не перехватили ли после нас какое-то прерывание
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
			
            mov es, word ptr cs:2Ch ;получим сегмент окружения DOS
            mov ah, 49h             ;функция освобождения памяти
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
	
	
	;"Чистка" и сохранение экрана
	printScr proc
	
	    push es ;Сохраняем сегментные регистры
        push ds
		
        mov cx, 0B800h
		push cx	;ds должен быть равен сегменту видеобуфера 0-ой страницы
        pop ds

        mov cx, 0B900h
		push cx	;es - 1-ой страницы
        pop es

        xor di, di	;Обнулим di и si
        xor si, si

        mov cx, 25 ;т.к. на экране 25 строк
        Next_line:
            push cx
            mov cx, 80 ;80 символов в строке
            Next_char:
                lodsw ;берем первый символ и атрибут в AX (lodsW)
                stosb ;заносим символ БЕЗ атрибута из AL в 1-ую видеостраницу (stosB)
            loop Next_char ;следующий символ 

            mov ax, 0A0Dh ;добавляем в конце каждой строки возврат каретки
            stosw ;перевод строки 
            pop cx
        loop Next_line	;cледующая строка

		pop ds
		pop es
		
		ret
		
	printScr endp
	
	;Запись в файл
	write_in_file proc
	
		pusha
		push cs ;ds должен быть равен сегменту, в котором мы сейчас находимся
        pop ds	

        mov byte ptr io_needed, 0 ;сбросили флаг требующейся записи на диск
		
        mov ah, 3Ch	
        xor cx, cx	
        mov dx, offset Screen_file	
        int 21h	
        push ax ;сохраним номер файла

        mov cx, 0B900h
		push cx	;ds должен указывать на 1-ую видеостраницу
        pop ds
        mov bx, ax ;в bx - номер файла
        mov ah, 40h	
        xor dx, dx ;ds:dx должны указывать на буфер, где находятся данные
        mov cx, 2050 ;кол-во записываемых байт (80*25 + 2*25 = 2050)
        int 21h	

		;Закроем файл
        pop bx	
        mov ah, 3Eh	
        int 21h
		
		popa
		ret
		
		Screen_file db 'screen.txt', 0 
	
	write_in_file endp
	
    ;Процедура safe_check
    ;Возвращает CF = 0, если в данный момент можно пользоваться функциями DOS,
    ;и CF = 1, если нельзя
    safe_check proc 
	
        push es
        push cs
        pop ds

        les di, dword ptr in_dos_addr ;адрес флагов занятости DOS,
        cmp word ptr es:[di], 0 ; если один из них не 0,
        pop es
        jne safe_check_failed ;пользоваться DOS нельзя

        clc ;CF = 0
        ret
		
        safe_check_failed:
            stc ;CF = 1
            ret
			
    safe_check endp
	
	in_dos_addr dd ? ;адрес флагов занятости DOS
	Crit_Seg dw 0
	Crit_Off dw 0 ;адрес критической ошибки
	io_needed db 0 ;1, если требуется запись на диск
	
	Init:
        call Get_cmd  ;проверим командную строку

        ;Если ничего в командной строке не введено, тогда пробуем установить резидент
        or al, al
        jz Ok_cmd 

        ;Если в командной строке введено '/u', то пробуем удалить программу из памяти
        cmp al, 1
        je Remove  

       ;В противном случае выведем сообщение о неверной командной строке и завершимся
       Bad_cmd:
           mov dx, offset Mbad_cmd
           mov ah, 09h
		   int 21h
		   
           ret
		   
		Ok_cmd:
	        ;Проверяем в памяти резидент или ещё нет?
            mov ax, 8899h 
            int 67h    
            cmp ax, 9988h 
            jne Set_resident

            mov ah, 09h ;если в памяти, то выведем соответствующее сообщение
            mov dx, offset In_memory 
            int 21h

            ret	 

        Set_resident:
            ;Если резидент не в памяти - установим его	
			
			;Получим флаг занятости DOS 
			mov ah, 34h
			int 21h
			dec bx
			
	        mov ah,5Dh 
            int 21h
            mov word ptr Crit_Seg, bx
            mov word ptr Crit_Off, es
			
			mov word ptr in_dos_addr, bx
			mov word ptr in_dos_addr+2, es 
			
			;Сначала 67h
            mov ax, 3567h
            int 21h
            mov word ptr Int_67h_vect, bx
            mov word ptr Int_67h_vect+2, es	

            mov ax, 2567h
            mov dx, offset Int_67h_proc
            int 21h	

			;Затем 09h
			mov ax, 3509h
            int 21h
            mov word ptr Int_09h_vect, bx	
            mov word ptr Int_09h_vect+2, es	

            mov ax, 2509h
            mov dx, offset Int_09h_proc
            int 21h
			
			;Теперь 28h
			mov ax, 3528h
            int 21h
            mov word ptr Int_28h_vect, bx	
            mov word ptr Int_28h_vect+2, es	

            mov ax, 2528h
            mov dx, offset Int_28h_proc
            int 21h
            		
			;Наконец 08h
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
		
    ;Обработка командной строки		
	Get_cmd proc
	
		;Получаем параметры командной строки
        mov si, 80h ;в si - смещение командной строки
        lodsb
        or al, al 
        jz Got_cmd 
        cmp al, 3     
        jne No_string ;на выход, если получили не 3 символа

        inc si     

		;Получаем сиволы и проверяем, соответствуют ли они последовательности /u
        Next_ch:
            lodsw  
            cmp ax, 'u/' ;данные приходят в обратном порядке, так что делаем такое сравнение
            jne No_string ;на выход, если не тот параметр
  
            mov al, 1 ;сигнал того, что пора удалять программу из памяти
            ret

        Got_cmd:
            xor al, al ;сигнал того, что была получена пустая командная строка
            ret 

        No_string:
            mov al, 3 ;сигнал того, что в командной строке произведён неверный ввод
            ret ;Выходим из процедуры
			
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