.model medium
public  inputline,input,readfile,output,writefile,menu,algorithm,words,space,compare,sort
extrn   start:far
extrn   lens:byte
extrn   ptrs:word
extrn   n:byte
extrn   delim:byte
    .code
inputline   proc
    locals @@
@@buffer    equ [bp+6]
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push di
    mov ah,3fh
    xor bx,bx
    mov cx,80
    mov dx,@@buffer
    int 21h
    jc @@ex
    cmp ax,80
    jne @@m
    stc
    jmp short @@ex
@@m:    mov di,@@buffer
    dec ax
    dec ax
    add di,ax
    xor al,al
    stosb
@@ex:   pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
    endp
input   proc
    locals @@
@@buffer    equ [bp+6]
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push di
    xor bx,bx
    mov cx,4095
    mov dx,@@buffer
@@m1:   mov ah,3fh
    int 21h
    jc @@ex
    cmp ax,2
    je @@m2
    sub cx,ax
    jcxz @@m2
    add dx,ax
    jmp @@m1
@@m2:   mov di,@@buffer
    add di,4095
    sub di,cx
    xor al,al
    stosb
@@ex:   pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
    endp
output  proc
    locals @@
@@buffer    equ [bp+6]
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push di
    mov di,@@buffer
    xor al,al
    mov cx,0ffffh
    repne scasb
    neg cx
    dec cx
    dec cx
    jcxz @@ex
    cmp cx,4095
    jbe @@m
    mov cx,4095
@@m:    mov ah,40h
    xor bx,bx
    inc bx
    mov dx,@@buffer
    int 21h
@@ex:   pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
    endp
readfile    proc
    locals @@
@@buffer    equ [bp+6]
@@filnam    equ [bp+8]
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push di
    mov ax,3d00h
    mov dx,@@filnam
    int 21h
    jc @@ex
    mov bx,ax
    mov cx,4095
    mov dx,@@buffer
@@m1:   mov ah,3fh
    int 21h
    jc @@er
    or ax,ax
    je @@m2
    sub cx,ax
    jcxz @@m2
    add dx,ax
    jmp @@m1
@@m2:   mov di,@@buffer
    add di,4095
    sub di,cx
    xor al,al
    stosb
    mov ah,3eh
    int 21h
@@ex:   pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
@@er:   mov ah,3eh
    int 21h
    stc
    jmp @@ex
    endp
writefile proc
    locals @@
@@filnam    equ [bp+8]
@@buffer    equ [bp+6]
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push di
    mov ah,3ch
    xor cx,cx
    mov dx,@@filnam
    int 21h
    jc @@ex
    mov bx,ax
    mov di,@@buffer
    xor al,al
    mov cx,0ffffh
    repne scasb
    neg cx
    dec cx
    dec cx
    jcxz @@ex
    cmp cx,4095
    jbe @@m
    mov cx,4095
@@m:    mov ah,40h
    mov dx,@@buffer
    int 21h
    jc @@er
    mov ah,3eh
    int 21h
@@ex:   pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
@@er:   mov ah,3eh
    int 21h
    stc
    jmp @@ex
    endp
menu    proc
    locals @@
@@ax        equ [bp-82]
@@buffer    equ [bp-80]
@@items equ [bp+6]
    push bp
    mov bp,sp
    sub sp,80
    push ax
@@m:    push @@items
    call output
    pop ax
    jc @@ex
    push ds
    push es
    push ss
    push ss
    pop ds
    pop es
    mov ax,bp
    sub ax,80
    push ax
    call inputline
    pop ax
    pop es
    pop ds
    jc @@ex
    mov al,@@buffer
    cbw
    sub ax,'0'
    cmp ax,0
    jl @@m
    cmp ax,@@ax
    jg @@m
    clc
@@ex:   mov sp,bp
    pop bp
    ret
    endp

space   proc          
    locals @@     
    push ax      
    push cx
    push di
    xor al,al
    mov cx,65535 
    repne scasb   
    neg cx      
    dec cx        
    push cx       
@@m1:   pop cx
    pop di
    push di
    push cx
    lodsb       
    repne scasb    
    jcxz @@m2    
    jmp @@m1      
@@m2:   dec si      
    add sp, 2     
    pop di
    pop cx
    pop ax        
    ret
space endp

words   proc       
    locals @@
    push ax
    push cx
    push di
    xor al,al
    mov cx,65535
    repne scasb
    neg cx
    push cx
@@m:    pop cx
    pop di
    push di
    push cx
    lodsb 
    repne scasb
    jcxz @@m
    dec si
    add sp,2
    pop di
    pop cx
    pop ax
    ret
    endp
compare proc
    locals @@
@@max equ [bp+6]
@@curr equ [bp+8]
@@lens equ [bp+10]
    push bp
    mov  bp, sp
    push bx
    push cx
    push si
    push di
    mov si,@@max ; max
    mov di,@@curr ; curr
    mov ax,@@lens
    cmp ah,al
    jg @@m1    ; ah > al : cl = al, else cl = ah
    mov cl,ah      
    jmp short @@m2
@@m1:   mov cl,al
@@m2:   xor ch,ch
    repe cmpsb
    je @@m4
@@m3:   jg @@m5
    jmp short @@m6
@@m4:   cmp ah,al
    jne @@m3
    xor ax,ax ; ah == al
    jmp short @@m7
@@m5:   mov ax,1  ; ah > al
    jmp short @@m7
@@m6:   mov ax,-1 ; ah < al 
@@m7:   pop di   
    pop si
    pop cx
    pop bx
    pop bp
    ret
    endp
sort    proc           
    locals @@
@@ptrs equ [bp+6]
@@lens equ [bp+8]
@@n equ [bp+10]
    push bp       
    mov bp, sp    
    push ax        
    push bx     
    push cx       
    push si       
    push di;6
    mov cx, @@n
        cmp bx, 1     
        je retu
    dec cx 
@@m1:   push cx  ;7     
    xor si,si    
    xor di,di      
@@m2:   inc di       
    mov bx,@@lens 
    mov ah,[bx+si]
    mov al,[bx+di]
    push ax           
    mov bx,@@ptrs
    add bx,di
    add bx,di
    push word ptr [bx] 
    mov bx,@@ptrs
    add bx,si
    add bx,si
    push word ptr [bx]   
    call compare  
    add sp,6 
    cmp ax,-1
    jne @@m3
    mov si,di
@@m3:   loop @@m2
    cmp si,di
    je @@m4
    mov bx,@@lens
    mov ah,[bx+si]     ; max -> ah
    xchg ah,[bx+di]    ; max -> curr; curr -> ah           
    mov [bx+si],ah     ; curr -> max
    mov bx,@@ptrs
    shl si,1
    shl di,1
    mov bx,@@ptrs
    push word ptr [bx+si]
    push word ptr [bx+di]
    pop word ptr [bx+si]
    pop word ptr [bx+di]
@@m4:   pop cx
    loop @@m1
retu:
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    pop bp
    ret
    endp
algorithm   proc
    locals @@
@@ibuf  equ [bp+6] 
@@obuf  equ [bp+8] 
    push bp 
    mov bp,sp
    push ax
    push bx
    push cx
    push si
    push di
    mov bx, @@obuf
    push bx
    mov si, @@ibuf
    lea di, delim
    xor bx, bx
@@m1: 
    call space              
    cmp byte ptr [si], 13
    je @@m2 
    shl bx,1           
    mov ptrs[bx], si   
    shr bx,1;          
    mov cx,si         
    call words         
    sub cx,si         
    neg cx
    mov lens[bx], cl   
    inc bx
    cmp byte ptr [si], 13  
    jne short @@m1                   
@@m2: 
    mov n,bl           
    push bx            
    push offset lens
    push offset ptrs
    call sort          
    add sp,6            
    mov cx,bx       
    xor bx,bx
    pop di
    push si
    jmp @@m3
@@mm1: jmp @@m1
@@m3: 
    push cx         
    or bx,bx          
    je @@m4
    mov al, ' '
    stosb                  
@@m4:
    shl bx, 1         
    mov si, ptrs[bx] 
    shr bx, 1
    mov cl, lens[bx]
    xor ch, ch
    rep movsb      
    inc bx           
    pop cx            
    loop @@m3
    mov al, 13
    stosb
    mov al, 10
    stosb
    pop si
    push di
    add si, 4
    cmp byte ptr [si], 0
    je @@ex
    dec si
    dec si
    xor bx, bx
    lea di, delim   
    jmp @@mm1
@@ex: pop dx  
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    pop bp
    ret
    endp
    
    end start
