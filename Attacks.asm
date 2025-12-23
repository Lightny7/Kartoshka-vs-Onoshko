proc ini
     GetTime
     mov [lastAttack], eax
     mov [lastShot], eax
     mov [bulletNumber], 0
     mov [circleNumber], 0
     ret
endp

proc iniLL
     call ini
     mov [circleNumber], 4
     ret
endp

proc iniTS
     GetTrueTime
     mov [lastAttack], eax
     mov [lastShot], eax
     mov [bulletNumber], 0
     mov [circleNumber], 0
     GetTrueTime
     add eax, TS_MOVE_DELAY-TS_MOVE_COOLDOWN
     mov [startTime], eax
     ret
endp

proc DefaultMove
     GetTime
     mov ecx, [startTime]
     add ecx, ENEMY_MOVE_COOLDOWN
     cmp eax, ecx
     jb .Skip
     mov [startTime], eax
     mov [aniTime], ENEMY_MOVE_TIME
     push [ex]
     pop [sex]
     push [ey]
     pop [sey]

     stdcall rGet, 500
     add eax, 1400
     mov [nex], eax
     stdcall rGet, 900
     add eax, 50
     mov [ney], eax
     mov eax, [OnoshkoMove]
     mov [OnoshkoImage], eax
     .Skip:
     ret
endp

proc TSMove
     GetTrueTime
     mov ecx, [startTime]
     add ecx, TS_MOVE_COOLDOWN
     cmp eax, ecx
     jl .Skip
     mov [startTime], eax
     mov [aniTime], TS_MOVE_TIME
     push [ex]
     pop [sex]
     push [ey]
     pop [sey]

     stdcall rGet, 100
     add eax, 1700
     mov [nex], eax
     stdcall rGet, 100
     add eax, 200
     cmp [ey], 500
     ja @F
      add eax, 700
     @@:
     mov [ney], eax
     mov eax, [OnoshkoMove]
     mov [OnoshkoImage], eax
     .Skip:
     ret
endp

proc LockMove
     cmp [ex], 1800
     jne .Go
     cmp [ey], 540
     jne .Go
     jmp .Skip
     .Go:
     cmp [nex], 1800
     je .Skip
     GetTime
     mov [startTime], eax
     mov [aniTime], ENEMY_MOVE_TIME
     push [ex]
     pop [sex]
     push [ey]
     pop [sey]

     mov [nex], 1800
     mov [ney], 540
     mov eax, [OnoshkoMove]
     .Skip:
     ret
endp

proc LLMove
     cmp [ex], 950
     jne .Go
     cmp [ey], 500
     jne .Go
     jmp .Skip
     .Go:
     cmp [nex], 950
     je .Skip
     GetTime
     mov [startTime], eax
     mov [aniTime], ENEMY_MOVE_TIME
     push [ex]
     pop [sex]
     push [ey]
     pop [sey]

     mov [nex], 950
     mov [ney], 500
     mov eax, [OnoshkoMove]
     .Skip:
     ret
endp

proc genRC
     GetTime
     sub eax, [lastAttack]
     cmp eax, RC_COOLDOWN
     jl .Skip
     GetTime
     mov [lastAttack], eax

     comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
     comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

     mov ebx, arrEB
     mov cx, [circleNumber]
     @@:
     test cx, cx
     jz @F
      add ebx, RC_BULLET_COUNT*EBULLET_SIZE
      dec cx
      jmp @B
     @@:
     GetTime
     xchg ecx, eax
     xor dx, dx
     mov edi, RC_BULLET_COUNT
     .Start:
      mov ax, word [ex]
      mov word [ebx + 5], ax
      mov ax, word [ey]
      mov word [ebx + 7], ax
      mov word [ebx + 9], dx

      mov dword [ebx], ecx
      mov byte [ebx + 4], 1

      add dx, 360/RC_BULLET_COUNT
      add ebx, EBULLET_SIZE
      dec edi
      jnz .Start
      inc [circleNumber]
      cmp [circleNumber], RC_CIRCLE_COUNT
      jb .Skip
       mov [circleNumber], 0
      .Skip:
     ret
endp

proc drawRC
     invoke SelectObject, [hdcImg], [BlueBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        GetTime
        sub eax, dword [ebx]
        sar eax, RC_SAR
        push eax
        mov [ftemp], eax

        fild word [ebx + 9]
        fmul [degToRad]
        fsincos
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        cmp edx, -SCREEN_CUTOFF
        jl .OoB
        cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .OoB
        cmp ecx, -SCREEN_CUTOFF
        jl .OoB
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .OoB
        stdcall DrawBullet, edx, ecx
        jmp .Loop
        .OoB:
         and byte [ebx + 4], 1111_1110b
      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genSO
     GetTime
     sub eax, [lastAttack]
     cmp eax, SO_COOLDOWN
     jl .Skip
     GetTime
     mov [lastAttack], eax

     cmp [shotNumber], SO_DUP_COUNT
     jne @F
      call Clear
      mov [shotNumber],0
      mov ax, [circleNumber]
      sub ax, [bulletNumber]
      mov [circleNumber], 0
      mov [bulletNumber], 0
      test ax, ax
      jz @F
       invoke MessageBox, NULL, _so, 0, 0
     @@:

     GetTime
     xchg ecx, eax
     cmp [shotNumber], 0
     jne .Normal
      movzx edi, [bulletNumber]
      imul edi, EBULLET_SIZE
      add edi, arrEB
      mov ax, word [ex]
      mov word [edi + 5], ax
      mov ax, word [ey]
      mov word [edi + 7], ax
      stdcall rGet, SO_START_SPREAD*2
      add eax, 270-SO_START_SPREAD
      mov word [edi + 9], ax

      mov dword [edi], ecx
      mov byte [edi + 4], 1
      inc [bulletNumber]
      mov [shotNumber], 1
      comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
      comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0
      jmp .Skip
     .Normal:
      movzx ebx, [bulletNumber]
      movzx edi, [bulletNumber]
      imul edi, EBULLET_SIZE
      add edi, arrEB
      movzx esi, [circleNumber]
      imul esi, EBULLET_SIZE
      add esi, arrEB
      .Out:
       test byte [esi + 4], 1
       push ebx
       jz .No
        GetTime
        push eax
        sub eax, dword [esi]
        sar eax, SO_SAR
        push eax
        mov [ftemp], eax

        fild word [esi + 9]
        fmul [degToRad]
        fsincos
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov ebx, [ftemp]

        movzx eax, word [esi + 5]
        add ebx, eax
        movzx eax, word [esi + 7]
        add ecx, eax

        pop eax
        cmp ebx, -SCREEN_CUTOFF
        jl .No
        cmp ebx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .No
        cmp ecx, -SCREEN_CUTOFF
        jl .No
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .No

        xor edx, edx
        .In:

        mov word [edi + 5], bx
        mov word [edi + 7], cx

        mov word [edi + 9], dx

        mov byte [edi + 4], 1
        mov dword [edi], eax

        add edi, EBULLET_SIZE
        inc [bulletNumber]
        cmp [bulletNumber], EBULLET_COUNT
        jb @F
         mov [bulletNumber], 0
         mov edi, arrEB
        @@:
        add dx, 360/SO_BULLET_COUNT
        cmp dx, 360
        jb .In
        mov byte [esi + 4], 0
       .No:
       pop ebx
       add esi, EBULLET_SIZE
       inc [circleNumber]
       cmp [circleNumber], EBULLET_COUNT
        jb @F
         mov [circleNumber], 0
         mov esi, arrEB
        @@:
       cmp [circleNumber], bx
       jb .Out
       inc [shotNumber]
       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0
      .Skip:
     ret
endp

proc drawSO
     invoke SelectObject, [hdcImg], [BlueBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        GetTime
        sub eax, dword [ebx]
        sar eax, SO_SAR
        push eax
        mov [ftemp], eax

        fild word [ebx + 9]
        fmul [degToRad]
        fsincos
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        stdcall DrawBullet, edx, ecx

      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genGS
     GetTime
     sub eax, [lastAttack]
     cmp eax, GS_END
     jl @F
      GetTime
      mov [lastAttack], eax
      mov eax, [OnoshkoIdle]
      mov [OnoshkoImage], eax
      jmp .Skip
     @@:
      cmp eax, GS_BEGIN
     jl .Skip
      mov eax, [OnoshkoShoot]
      mov [OnoshkoImage], eax
      GetTime
      sub eax, [lastShot]
      cmp eax, GS_COOLDOWN
     jl .Skip
      mov ebx, arrEB
      movzx ecx, [bulletNumber]
      test ecx, ecx
      jz .noZeroes                                                 ;Оптимизация в кринже, нужен
      @@:
       add ebx, EBULLET_SIZE
      loop @B
      .noZeroes:
      @@:
       mov ax, word [ex]
       sub ax, 25
       mov word [ebx + 5], ax
       mov ax, word [ey]
       sub ax, 95
       mov word [ebx + 7], ax
       GetTime
       mov dword [ebx], eax
       mov [lastShot], eax
       mov byte [ebx + 4], 1

       mov edx, [x]
       stdcall rGet, GS_SPREAD
       sub eax, GS_SPREAD/2
       add edx, eax
       sub edx, [ex]
       mov dword [ebx + 9], edx
       mov ecx, [y]
       stdcall rGet, GS_SPREAD
       sub eax, GS_SPREAD/2
       add ecx, eax
       sub ecx, [ey]
       mov dword [ebx + 13], ecx
       imul edx, edx
       imul ecx, ecx
       add edx, ecx
       mov [ftemp], edx
       fild [ftemp]
       fsqrt
       fstp [ftemp]
       fild dword [ebx + 9]
       fdiv [ftemp]
       fstp dword [ebx + 9]
       fild dword [ebx + 13]
       fdiv [ftemp]
       fstp dword [ebx + 13]
       stdcall rGet, GS_SPEED_SPREAD
       add al, GS_SPEED_MIN
       mov byte [ebx + 17], al

       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

       inc [bulletNumber]
       cmp [bulletNumber], EBULLET_COUNT
       jb @F
        mov [bulletNumber], 0
       @@:
     .Skip:
     ret
endp

proc drawGS
     invoke SelectObject, [hdcImg], [GreenBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        GetTime
        sub eax, dword [ebx]
        movzx ecx, byte [ebx + 17]
        imul eax, ecx
        sar eax, GS_SAR
        push eax
        mov [ftemp], eax

        fld dword [ebx + 9]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]
        fld dword [ebx + 13]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        cmp edx, -SCREEN_CUTOFF
        jl .OoB
        cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .OoB
        cmp ecx, -SCREEN_CUTOFF
        jl .OoB
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .OoB
        stdcall DrawBullet, edx, ecx
        jmp .Loop
        .OoB:
         and byte [ebx + 4], 1111_1110b
      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genLL
     GetTime
     sub eax, [lastAttack]
     cmp eax, LL_COOLDOWN1
     jl .Skip1
     GetTime
     mov [lastAttack], eax

     comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
     comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

     mov ebx, arrEB
      movzx ecx, [bulletNumber]
      test ecx, ecx
      jz .noZeroes                                                 ;Оптимизация в кринже, нужен
      @@:
       add ebx, EBULLET_SIZE
      loop @B
      .noZeroes:
     GetTime
     xchg ecx, eax
     xor dx, dx
     mov edi, LL_BULLET_COUNT1
     .Start:
      movzx eax, [circleNumber]
      imul eax, LL_WINDOW_SIZE
      cmp edx, eax
      jb .Draw
      add eax, LL_WINDOW_SIZE
      cmp eax, 0
      cmp edx, eax
      jb .Window
      .Draw:
       mov ax, word [ex]
       mov word [ebx + 5], ax
       mov ax, word [ey]
       mov word [ebx + 7], ax
       mov word [ebx + 9], dx

       mov dword [ebx], ecx
       mov byte [ebx + 4], 01b

      .Window:
      add dx, 360/LL_BULLET_COUNT1
      add ebx, EBULLET_SIZE
      dec edi
      jnz .Start
      inc [circleNumber]
      cmp [circleNumber], 360/LL_WINDOW_SIZE
      jb @F
       mov [circleNumber], 0
      @@:
      add [bulletNumber], LL_BULLET_COUNT1
      cmp [bulletNumber], LL_BULLET_COUNT
      jb .Skip1
       mov [bulletNumber], 0
     .Skip1:

     GetTime
     sub eax, [lastShot]
     cmp eax, LL_COOLDOWN2
     jl .Skip2
     GetTime
     mov [lastShot], eax

     comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
     comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

     mov ebx, arrEB
      movzx ecx, [bulletNumber]
      test ecx, ecx
      jz .noZeroes2                                                 ;Оптимизация в кринже, нужен
      @@:
       add ebx, EBULLET_SIZE
      loop @B
      .noZeroes2:
     GetTime
     xchg ecx, eax
     xor dx, dx
     mov edi, LL_BULLET_COUNT2
     .Start2:
      mov ax, word [ex]
      mov word [ebx + 5], ax
      mov ax, word [ey]
      mov word [ebx + 7], ax
      mov word [ebx + 9], dx

      mov dword [ebx], ecx
      mov byte [ebx + 4], 11b

      add dx, 360/LL_BULLET_COUNT2
      add ebx, EBULLET_SIZE
      dec edi
      jnz .Start2
      add [bulletNumber], LL_BULLET_COUNT2
      cmp [bulletNumber], LL_BULLET_COUNT
      jb @F
       mov [bulletNumber], 0
      @@:
     .Skip2:
     ret
endp

proc drawLL
     invoke SelectObject, [hdcImg], [GreenBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        movzx edx, word [ebx + 5]
        movzx ecx, word [ebx + 7]

        GetTime
        sub eax, dword [ebx]
        test byte [ebx + 4], 10b
        jnz @F
         sar eax, LL_SAR1
         jmp .end
        @@:
         sar eax, LL_SAR2
        .end:
        push eax
        mov [ftemp], eax

        fild word [ebx + 9]
        fmul [degToRad]
        fsincos
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        cmp edx, -SCREEN_CUTOFF
        jl .OoB
        cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .OoB
        cmp ecx, -SCREEN_CUTOFF
        jl .OoB
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .OoB
        stdcall DrawBullet, edx, ecx
        jmp .Loop
        .OoB:
         and byte [ebx + 4], 1111_1110b
      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genBB
     GetTime
     sub eax, [lastAttack]
     cmp eax, BB_COOLDOWN
     jl .Skip
      GetTime
      mov [lastAttack], eax
      mov eax, [OnoshkoShoot]
      mov [OnoshkoImage], eax
      mov ebx, arrEB
      movzx eax, [bulletNumber]
      imul eax, EBULLET_SIZE
      add ebx, eax
      movzx esi, word [ex]
      sub esi, 25
      movzx edi, word [ey]
      sub edi, 95
      mov dl, BB_MINSPEED
      .Loop:
       mov word [ebx + 5], si
       mov word [ebx + 7], di
       push edx
       GetTime
       pop edx
       mov dword [ebx], eax
       mov byte [ebx + 4], 1

       mov eax, [x]
       sub eax, esi
       mov dword [ebx + 9], eax
       mov ecx, [y]
       sub ecx, edi
       mov dword [ebx + 13], ecx
       imul eax, eax
       imul ecx, ecx
       add eax, ecx
       mov [ftemp], eax
       fild [ftemp]
       fsqrt
       fstp [ftemp]
       fild dword [ebx + 9]
       fdiv [ftemp]
       fstp dword [ebx + 9]
       fild dword [ebx + 13]
       fdiv [ftemp]
       fstp dword [ebx + 13]
       mov byte [ebx + 17], dl
       push edx

       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

       pop edx
       add ebx, EBULLET_SIZE
       inc [bulletNumber]
       cmp [bulletNumber], EBULLET_COUNT
       jb @F
        mov [bulletNumber], 0
        mov ebx, arrEB
       @@:
       inc dl
       cmp dl, BB_MAXSPEED
       jbe .Loop
     .Skip:
     ret
endp

proc drawBB
     invoke SelectObject, [hdcImg], [RedBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        GetTime
        sub eax, dword [ebx]
        movzx ecx, byte [ebx + 17]
        imul eax, ecx
        sar eax, BB_SAR
        push eax
        mov [ftemp], eax

        fld dword [ebx + 9]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]
        fld dword [ebx + 13]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        cmp edx, -SCREEN_CUTOFF
        jl .OoB
        cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .OoB
        cmp ecx, -SCREEN_CUTOFF
        jl .OoB
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .OoB
        stdcall DrawBullet, edx, ecx
        jmp .Loop
        .OoB:
         and byte [ebx + 4], 1111_1110b
      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genTS
     GetTrueTime
     sub eax, [lastAttack]
     cmp eax, TS_STOP_COOLDOWN+TS_STOP_DURATION
     jl @F
      GetTrueTime
      mov [lastAttack], eax
      mov eax, [OnoshkoIdle]
      mov [OnoshkoImage], eax
      test [timeState], 1b
      jz .No1
       call TimeUnlock
      .No1:
      jmp .Skip
     @@:
      cmp eax, TS_STOP_COOLDOWN
     jl .Skip
      mov eax, [OnoshkoShoot]
      mov [OnoshkoImage], eax
      test [timeState], 1b
      jnz .No2
       call TimeLock
       comcall [pDsbZaWarudo], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbZaWarudo], IDirectSoundBuffer, Play, 0, 0, 0
      .No2:
      GetTrueTime
      sub eax, [lastShot]
      cmp eax, TS_BURST_COOLDOWN
     jl .Skip
      mov ebx, arrEB
      movzx eax, [bulletNumber]
      imul eax, EBULLET_SIZE
      add ebx, eax
      mov esi, [ex]
      sub esi, 25
      mov edi, [ey]
      sub edi, 95
      mov edx, TS_BURST_SIZE
      .Loop:
       push edx
       mov word [ebx + 5], si
       mov word [ebx + 7], di
       GetTime
       mov dword [ebx], eax
       GetTrueTime
       mov [lastShot], eax
       mov byte [ebx + 4], 1

       mov ecx, [x]
       stdcall rGet, TS_SPREAD
       sub eax, TS_SPREAD/2
       add ecx, eax
       sub ecx, esi
       mov dword [ebx + 9], ecx
       push ecx
       mov ecx, [y]
       stdcall rGet, TS_SPREAD
       sub eax, TS_SPREAD/2
       add ecx, eax
       sub ecx, edi
       mov dword [ebx + 13], ecx
       pop eax
       imul eax, eax
       imul ecx, ecx
       add eax, ecx
       mov [ftemp], eax
       fild [ftemp]
       fsqrt
       fstp [ftemp]
       fild dword [ebx + 9]
       fdiv [ftemp]
       fstp dword [ebx + 9]
       fild dword [ebx + 13]
       fdiv [ftemp]
       fstp dword [ebx + 13]
       stdcall rGet, TS_SPEED_SPREAD
       add al, TS_SPEED_MIN
       mov byte [ebx + 17], al

       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

       add ebx, EBULLET_SIZE
       inc [bulletNumber]
       cmp [bulletNumber], EBULLET_COUNT
       jb @F
        mov [bulletNumber], 0
        mov ebx, arrEB
       @@:
       pop edx
       dec edx
       jnz .Loop
     .Skip:
     ret
endp

proc drawTS
     invoke SelectObject, [hdcImg], [RedBull]
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Start:
        test byte [ebx + 4], 1
        jz .Loop

        GetTime
        sub eax, dword [ebx]
        movzx ecx, byte [ebx + 17]
        imul eax, ecx
        sar eax, TS_SAR
        push eax
        mov [ftemp], eax

        fld dword [ebx + 9]
        fimul [ftemp]
        fistp [ftemp]
        mov edx, [ftemp]
        fld dword [ebx + 13]
        pop [ftemp]
        fimul [ftemp]
        fistp [ftemp]
        mov ecx, [ftemp]

        movzx eax, word [ebx + 5]
        add edx, eax
        movzx eax, word [ebx + 7]
        add ecx, eax

        cmp edx, -SCREEN_CUTOFF
        jl .OoB
        cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
        jg .OoB
        cmp ecx, -SCREEN_CUTOFF
        jl .OoB
        cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
        jg .OoB
        stdcall DrawBullet, edx, ecx
        jmp .Loop
        .OoB:
         and byte [ebx + 4], 1111_1110b
      .Loop:
      add ebx, EBULLET_SIZE
      sub edi, 1
      jnz .Start
      ret
endp

proc genFin
     GetTime
     mov ecx, eax
     sub eax, [lastAttack]
     sub eax, LAST_MODE_DURATION
     cmp eax, [EHP]
     jl .NoChange
      mov [lastAttack], ecx
      inc [circleNumber]
      cmp [circleNumber], 2
      jbe @F
       mov [circleNumber], 0
      @@:
      movzx eax, [circleNumber]
      stdcall SetPulse, [Delphi + eax*4], [ex], [ey]
     .NoChange:
     movzx ebx, [bulletNumber]
     imul ebx, EBULLET_SIZE
     add ebx, arrEB
     cmp [circleNumber], 1
     je .Green
     ja .Blue
     .Red:
      GetTime
      mov ecx, eax
      sub eax, [lastShot]
      cmp eax, LRED_COOLDOWN
      jb .End
       mov [lastShot], ecx
       push [OnoshkoShoot]
       pop [OnoshkoImage]
       mov esi, [ex]
       mov edi, [ey]
       sub esi, 25
       sub edi, 95
       mov dl, LRED_MINSPEED
       .Loop:
        mov word [ebx + 5], si
        mov word [ebx + 7], di
        push edx
        GetTime
        pop edx
        mov dword [ebx], eax
        mov byte [ebx + 4], 001b

        mov eax, [x]
        sub eax, esi
        mov dword [ebx + 9], eax
        mov ecx, [y]
        sub ecx, edi
        mov dword [ebx + 13], ecx
        imul eax, eax
        imul ecx, ecx
        add eax, ecx
        mov [ftemp], eax
        fild [ftemp]
        fsqrt
        fstp [ftemp]
        fild dword [ebx + 9]
        fdiv [ftemp]
        fstp dword [ebx + 9]
        fild dword [ebx + 13]
        fdiv [ftemp]
        fstp dword [ebx + 13]
        mov byte [ebx + 17], dl
        push edx

        comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
        comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

        pop edx
        add ebx, EBULLET_SIZE
        inc [bulletNumber]
        cmp [bulletNumber], EBULLET_COUNT
        jb @F
         mov [bulletNumber], 0
         mov ebx, arrEB
        @@:
        inc dl
        cmp dl, LRED_MAXSPEED
        jbe .Loop
       jmp .End
     .Green:
      GetTime
      mov ecx, eax
      sub eax, [lastShot]
      mov esi, [ex]
      mov edi, [ey]
      sub esi, 25
      sub edi, 95
      cmp eax, LGREEN_COOLDOWN
      jb .End
       mov [lastShot], ecx
       push [OnoshkoShoot]
       pop [OnoshkoImage]
       mov word [ebx + 5], si
       mov word [ebx + 7], di
       mov dword [ebx], ecx
       mov byte [ebx + 4], 011b

       mov edx, [x]
       stdcall rGet, LGREEN_SPREAD
       sub eax, LGREEN_SPREAD/2
       add edx, eax
       sub edx, esi
       mov dword [ebx + 9], edx
       mov ecx, [y]
       stdcall rGet, LGREEN_SPREAD
       sub eax, LGREEN_SPREAD/2
       add ecx, eax
       sub ecx, edi
       mov dword [ebx + 13], ecx
       imul edx, edx
       imul ecx, ecx
       add edx, ecx
       mov [ftemp], edx
       fild [ftemp]
       fsqrt
       fstp [ftemp]
       fild dword [ebx + 9]
       fdiv [ftemp]
       fstp dword [ebx + 9]
       fild dword [ebx + 13]
       fdiv [ftemp]
       fstp dword [ebx + 13]
       stdcall rGet, LGREEN_SPEED_SPREAD
       add al, LGREEN_SPEED_MIN
       mov byte [ebx + 17], al

       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

       inc [bulletNumber]
       cmp [bulletNumber], EBULLET_COUNT
       jb @F
        mov [bulletNumber], 0
       @@:
      jmp .End
     .Blue:
      GetTime
      mov ecx, eax
      sub eax, [lastShot]
      cmp eax, LBLUE_COOLDOWN
      jb .End
       mov [lastShot], ecx
       mov esi, ecx
       push [OnoshkoIdle]
       pop [OnoshkoImage]
       stdcall rGet, LBLUE_MAX_OFFSET
       xchg edx, eax
       .Start:
        mov ax, word [ex]
        mov word [ebx + 5], ax
        mov ax, word [ey]
        mov word [ebx + 7], ax
        mov word [ebx + 9], dx

        mov dword [ebx], esi
        mov byte [ebx + 4], 101b

        add dx, 360/LBLUE_CIRCLE_SIZE
        add ebx, EBULLET_SIZE
        inc [bulletNumber]
        cmp dx, 360+LBLUE_MAX_OFFSET
       jbe .Start
       comcall [pDsbShoot], IDirectSoundBuffer, SetCurrentPosition, 0
       comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0
     .End:
     ret
endp

proc drawFin
     mov ebx, arrEB
     mov edi, EBULLET_COUNT
     .Loop:
      test byte [ebx + 4], 1b
      jz .End
      test byte [ebx + 4], 100b
      jnz .Blue
      test byte [ebx + 4], 10b
      jnz .Green
      .Red:
       invoke SelectObject, [hdcImg], [RedBull]
       jmp .Vector
      .Green:
       invoke SelectObject, [hdcImg], [GreenBull]
      .Vector:
       GetTime
       sub eax, dword [ebx]
       movzx ecx, byte [ebx + 17]
       imul eax, ecx
       test byte [ebx + 4], 10b
       jnz @F
        sar eax, LRED_SAR
        jmp .Cont
       @@:
        sar eax, LGREEN_SAR
       .Cont:
       push eax
       mov [ftemp], eax

       fld dword [ebx + 9]
       fimul [ftemp]
       fistp [ftemp]
       mov edx, [ftemp]
       fld dword [ebx + 13]
       pop [ftemp]
       fimul [ftemp]
       fistp [ftemp]
       mov ecx, [ftemp]

       movzx eax, word [ebx + 5]
       add edx, eax
       movzx eax, word [ebx + 7]
       add ecx, eax

       jmp .Draw
      .Blue:
       invoke SelectObject, [hdcImg], [BlueBull]
       GetTime
       sub eax, dword [ebx]
       sar eax, LBLUE_SAR
       push eax
       mov [ftemp], eax

       fild word [ebx + 9]
       fmul [degToRad]
       fsincos
       fimul [ftemp]
       fistp [ftemp]
       mov ecx, [ftemp]
       pop [ftemp]
       fimul [ftemp]
       fistp [ftemp]
       mov edx, [ftemp]

       movzx eax, word [ebx + 5]
       add edx, eax
       movzx eax, word [ebx + 7]
       add ecx, eax

     .Draw:
       cmp edx, -SCREEN_CUTOFF
       jl .OoB
       cmp edx, NATIVE_WIDTH + SCREEN_CUTOFF
       jg .OoB
       cmp ecx, -SCREEN_CUTOFF
       jl .OoB
       cmp ecx, NATIVE_HEIGHT + SCREEN_CUTOFF
       jg .OoB
       stdcall DrawBullet, edx, ecx
       jmp .End
       .OoB:
        and byte [ebx + 4], 1111_1110b
     .End:
      add ebx, EBULLET_SIZE
      dec edi
      jne .Loop
     ret
endp