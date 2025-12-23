format PE GUI 4.0
entry start

include 'win32w.inc'                      ;Звук зависим!!!!!!!!
include 'ENCODING\WIN1251.INC'
include 'DSOUND.INC'

include 'Config.asm'

struct Attack
       name dd ?
       nameLen dd ?
       iniProc dd ?
       genProc dd ?
       drawProc dd ?
       moveProc dd ?
ends

macro GetTime
{
      call fGetTime
}
macro GetTrueTime
{
      invoke TimeGetTime
}

section '.text' code readable executable

  start:
        invoke  GetModuleHandle,0
        mov     [wc.hInstance],eax
        invoke  LoadImage,0, _icon, IMAGE_ICON,0,0, LR_LOADFROMFILE+LR_SHARED
        mov     [Icon], eax
        mov     [wc.hIcon],eax
        invoke  LoadCursor,0,IDC_ARROW
        mov     [wc.hCursor],eax
        invoke  RegisterClass,wc
        test    eax,eax
        jz      error

        invoke  CreateWindowEx,0,_class,_title,WS_VISIBLE+WS_POPUP+WS_MAXIMIZE,0,0,1920,1080,NULL,NULL,[wc.hInstance],NULL
        test    eax,eax
        jz      error

  msg_loop:
        invoke  GetMessage,msg,NULL,0,0
        cmp     eax,1
        jb      end_loop
        jne     msg_loop
        invoke  TranslateMessage,msg
        invoke  DispatchMessage,msg
        jmp     msg_loop

  error:
        invoke  MessageBox,NULL,_error,NULL,MB_ICONERROR+MB_OK

  end_loop:
        invoke  ExitProcess,[msg.wParam]

proc WindowProc uses ebx esi edi, hwnd,wmsg,wparam,lparam
        cmp     [wmsg],WM_DESTROY
        je      .wmdestroy
        cmp     [wmsg],WM_CREATE
        je      .wmcreate
        cmp     [wmsg],WM_PAINT
        je      .wmpaint
        cmp     [wmsg],WM_KEYDOWN
        je      .wmkeydown
        cmp     [wmsg],WM_KEYUP
        je      .wmkeyup
  .defwndproc:
        invoke  DefWindowProc,[hwnd],[wmsg],[wparam],[lparam]
        jmp     .finish
  .wmcreate:
        invoke GetDC, [hwnd]
        mov [hdc], eax

        invoke CreateCompatibleDC, eax
        mov [hdcImg], eax

        call LoadImages

        invoke CreateSolidBrush, BACKGROUND_COLOR
        mov [Brush], eax

        invoke CreateSolidBrush, HEALTHBAR_COLOR
        mov [RedBrush], eax

        invoke CreateSolidBrush, SC_HEALTHBAR_COLOR
        mov [GrayBrush], eax

        invoke CreateSolidBrush, BG_HEALTHBAR_COLOR
        mov [DGrayBrush], eax

        invoke CreateFont, 40, 0, 0, 0, 0, 1, 0, 0, RUSSIAN_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH+FF_DONTCARE, 0
        mov [Font], eax

        invoke CreateCompatibleDC, [hdc]
        mov [hdcBuf], eax

        invoke CreateCompatibleBitmap, [hdc], NATIVE_WIDTH, NATIVE_HEIGHT
        mov [bmMem], eax

        invoke SelectObject, [hdcBuf], [bmMem]

        invoke SelectObject, [hdcBuf], [Font]

        invoke SetBkMode, [hdcBuf], TRANSPARENT

        invoke SetTextColor, [hdcBuf], 00FFFFFFh

        invoke SetStretchBltMode, [hdc], COLORONCOLOR

        ;Еее, дайрект саунд!
        invoke DirectSoundCreate, NULL, pDSound, NULL

        comcall [pDSound], IDirectSound, SetCooperativeLevel, [hwnd], DSSCL_PRIORITY


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbDeath, NULL

        invoke CreateFile, _death, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbDeath], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbDeath], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbShoot, NULL

        invoke CreateFile, _shoot, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbShoot], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbShoot], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbZaWarudo, NULL

        invoke CreateFile, _timestop, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbZaWarudo], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbZaWarudo], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbSpellcard, NULL

        invoke CreateFile, _spellcard, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbSpellcard], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbSpellcard], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbCheers, NULL

        invoke CreateFile, _cheers, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbCheers], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbCheers], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDSound], IDirectSound, CreateSoundBuffer, dsbdesc, pDsbVoice, NULL

        invoke CreateFile, _voice, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
        mov [hFile], eax

        comcall [pDsbVoice], IDirectSoundBuffer, Lock, 0, 0xFFFFF, pb1, lb1, pb2, lb2, 0

        invoke ReadFile, [hFile], [pb1], [lb1], lb1, NULL

        comcall [pDsbVoice], IDirectSoundBuffer, Unlock, [pb1], [lb1], [pb2], 0

        invoke CloseHandle, [hFile]


        comcall [pDsbShoot], IDirectSoundBuffer, Play, 0, 0, 0

        GetTime
        mov    [lastFrame], eax
        mov    [bulletCooldown], eax
        mov    [nextAttack], eax

        call rIni

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks + Attack.iniProc
        call dword [eax]

        fldpi
        fidiv [degToRad]
        fstp [degToRad]

        jmp .defwndproc
  .wmpaint:
        GetTime
        mov ebx, eax

        call ManageInputs

        mov [lastFrame], ebx

        call FPSCounter

        invoke BeginPaint, [hwnd], ps

        invoke FillRect, [hdcBuf], fullRect, [Brush]

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks + Attack.nameLen
        cmp dword [eax], 0
        je @F
        call drawBg
        @@:

        cmp [pulseImg], 0
        je @F
         stdcall DrawPulse, [pulseImg], [pulseX], [pulseY], [pulseT], PULSE_WIDTH, PULSE_HEIGHT, PULSE_PEAK, PULSE_FADE, PULSE_DURATION,\
                            ICON_WIDTH, ICON_HEIGHT, SRCPAINT
        @@:

        call DeleteBullets

        call DrawBullets

        mov eax, [threshold]
        cmp [EHP], eax
        jg @F
         call ChangeAttack
        @@:

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks + Attack.genProc
        call dword [eax]

        invoke SelectObject, [hdcImg], [Potato]

        mov eax, [x]
        sub eax, PLAYER_WIDTH/2
        mov ecx, [y]
        sub ecx, PLAYER_HEIGHT/2

        invoke TransparentBlt, [hdcBuf], eax, ecx, PLAYER_WIDTH, PLAYER_HEIGHT, [hdcImg], 0, 0, POTATO_WIDTH, POTATO_HEIGHT, 0x00FF00FF

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks + Attack.drawProc
        call dword [eax]

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks + Attack.moveProc
        call dword [eax]

        call DrawEnemy

        stdcall DrawPulse, [Explosion], [expx], [expy], [hitTime], EXPL_WIDTH, EXPL_HEIGHT, EXPL_PEAK, EXPL_FADE, EXPL_DURATION,\
                            EXPLOSION_WIDTH, EXPLOSION_HEIGHT, SRCPAINT

        stdcall DrawPulse, [Explosion], [eexpX], [eexpY], [eexpT], EXPL_WIDTH, EXPL_HEIGHT, EXPL_PEAK, EXPL_FADE, EXPL_DURATION,\
                            EXPLOSION_WIDTH, EXPLOSION_HEIGHT, SRCPAINT

        call DrawSplash

        imul eax, [currAttack], sizeof.Attack
        add eax, Attacks

        invoke TextOut, [hdcBuf], 1000, 30, [eax], [eax + 4]

        mov ebx, [HP]
        invoke SelectObject, [hdcImg], [Potato]
        mov edi, 30
        @@:
         invoke TransparentBlt, [hdcBuf], edi, 30, POTATO_WIDTH, POTATO_HEIGHT, [hdcImg], 0, 0, POTATO_WIDTH, POTATO_HEIGHT, 0x00FF00FF
         add edi, POTATO_WIDTH+20
        dec ebx
        jg @B

        cmp [EHP], 0
        jl .NoHB
        cmp [currAttack], LAST_ATTACK
        je .SpecialHB

        invoke SelectObject, [hdcBuf], [DGrayBrush]

        invoke Rectangle, [hdcBuf], 5, 1060, RIGHT_HB, 1070

        cmp [EHP], SC_THRESHOLD
        jb @F

        invoke SelectObject, [hdcBuf], [RedBrush]

        fild [EHP]
        fidiv [MEHP]
        mov [ftemp], RIGHT_HB
        fimul [ftemp]
        fistp [ftemp]

        invoke Rectangle, [hdcBuf], 5, 1060, [ftemp], 1070

        @@:

        invoke SelectObject, [hdcBuf], [GrayBrush]

        cmp [EHP], SC_THRESHOLD
        jb @F
         mov [ftemp], SC_THRESHOLD*RIGHT_HB/ENEMY_HEALTH
         jmp .Fin
        @@:
         fild [EHP]
         fidiv [MEHP]
         mov [ftemp], RIGHT_HB
         fimul [ftemp]
         fistp [ftemp]

        .Fin:

        invoke Rectangle, [hdcBuf], 5, 1060, [ftemp], 1070
        jmp .Text
        .SpecialHB:
         invoke SelectObject, [hdcBuf], [GrayBrush]

         fild [EHP]
         fidiv [MEHP]
         mov [ftemp], RIGHT_HB
         fimul [ftemp]
         fistp [ftemp]

         invoke Rectangle, [hdcBuf], 5, 1060, [ftemp], 1070

        .Text:

        invoke TextOut, [hdcBuf], RIGHT_HB+20, 1030, _hb, 1

        .NoHB:

        invoke TextOut, [hdcBuf], 1700, 30, _fps, [fpsLen]

        invoke StretchBlt, [hdc], 0,0, [ps.rcPaint.right], [ps.rcPaint.bottom], [hdcBuf], 0,0, NATIVE_WIDTH, NATIVE_HEIGHT, SRCCOPY

        call Ending

        invoke EndPaint, [hwnd], ps

        invoke InvalidateRect, [hwnd], NULL, FALSE

     .Skip:

        xor eax, eax
        jmp .finish
  .wmkeydown:
        cmp [wparam], VK_LEFT
        je .leftd
        cmp [wparam], VK_RIGHT
        je .rightd
        cmp [wparam], VK_UP
        je .upd
        cmp [wparam], VK_DOWN
        je .downd
        cmp [wparam], VK_SHIFT
        je .shiftd
        cmp [wparam], 'Z'
        je .zd
        jmp .defwndproc
  .leftd:
        or [input], 1b
        jmp .defwndproc
  .rightd:
        or [input], 10b
        jmp .defwndproc
  .upd:
        or [input], 100b
        jmp .defwndproc
  .downd:
        or [input], 1000b
        jmp .defwndproc
  .zd:
        or [input], 1_0000b
        jmp .defwndproc
  .shiftd:
        mov [speed], FOCUS_SPEED
        jmp .defwndproc
  .wmkeyup:
        cmp [wparam], VK_LEFT
        je .left
        cmp [wparam], VK_RIGHT
        je .right
        cmp [wparam], VK_UP
        je .up
        cmp [wparam], VK_DOWN
        je .down
        cmp [wparam], 'Z'
        je .z
        cmp [wparam], VK_SHIFT
        je .shift
        jmp .defwndproc
  .left:
        and [input], 1111_1110b
        jmp .defwndproc
  .right:
        and [input], 1111_1101b
        jmp .defwndproc
  .up:
        and [input], 1111_1011b
        jmp .defwndproc
  .down:
        and [input], 1111_0111b
        jmp .defwndproc
  .z:
        and [input], 1110_1111b
        jmp .defwndproc
  .shift:
        mov [speed], BASE_SPEED
        jmp .defwndproc
  .wmdestroy:
        call FreeResources
        invoke PostQuitMessage,0
        xor    eax,eax
  .finish:
        ret
endp

proc LoadImages
        invoke LoadImage, NULL, _potato, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [Potato], eax

        invoke LoadImage, NULL, _onoshkoidle, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [OnoshkoIdle], eax
        mov [OnoshkoImage], eax

        invoke LoadImage, NULL, _onoshkomove, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [OnoshkoMove], eax

        invoke LoadImage, NULL, _onoshkoshoot, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [OnoshkoShoot], eax

        invoke LoadImage, NULL, _redbull, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [RedBull], eax

        invoke LoadImage, NULL, _greenbull, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [GreenBull], eax

        invoke LoadImage, NULL, _bluebull, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [BlueBull], eax

        invoke LoadImage, NULL, _zero, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [Zero], eax

        invoke LoadImage, NULL, _one, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [One], eax

        invoke LoadImage, NULL, _bgzero, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [bgZero], eax

        invoke LoadImage, NULL, _bgone, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [bgOne], eax

        invoke LoadImage, NULL, _explosion, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [Explosion], eax

        invoke LoadImage, NULL, _splash, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [Splash], eax

        invoke LoadImage, NULL, _C, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [C], eax

        invoke LoadImage, NULL, _delphi, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [Delphi], eax

        invoke LoadImage, NULL, _fasm, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE
        mov [fasm], eax
        ret
endp

proc EndMessageBox
     invoke MessageBox, NULL, _congrats, _congrats, MB_TASKMODAL
     ret
endp

proc FreeResources
     invoke DeleteDC, [hdcBuf]
     invoke DeleteDC, [hdcImg]
     call DeleteImages
     invoke DeleteObject, [Font]
     invoke DeleteObject, [Brush]
     invoke DeleteObject, [RedBrush]
     invoke DeleteObject, [GrayBrush]
     invoke DeleteObject, [DGrayBrush]
     invoke DeleteObject, [bmMem]
     invoke DestroyIcon, [Icon]
     ret
endp

proc DeleteImages
     invoke DeleteObject, [Potato]
     invoke DeleteObject, [OnoshkoIdle]
     invoke DeleteObject, [OnoshkoMove]
     invoke DeleteObject, [OnoshkoShoot]
     invoke DeleteObject, [RedBull]
     invoke DeleteObject, [GreenBull]
     invoke DeleteObject, [BlueBull]
     invoke DeleteObject, [Zero]
     invoke DeleteObject, [One]
     invoke DeleteObject, [bgZero]
     invoke DeleteObject, [bgOne]
     invoke DeleteObject, [Explosion]
     invoke DeleteObject, [Splash]
     invoke DeleteObject, [C]
     invoke DeleteObject, [Delphi]
     invoke DeleteObject, [fasm]
     ret
endp

proc FPSCounter
     inc [fpsCounter]
     GetTime
     sub eax, [lastSecond]
     cmp eax, 1000
     jb @F
      xor eax, eax
      xchg eax, [fpsCounter]
      stdcall IntToStr, _fps
      mov [fpsLen], eax
      GetTime
      mov [lastSecond], eax
     @@:
     ret
endp

proc Ending
     cmp [currAttack], LAST_ATTACK
        jbe @F
         GetTime
         cmp eax, [endTimer]
         jbe @F
          comcall [pDsbVoice], IDirectSoundBuffer, Play, 0, 0, 0
          invoke MessageBox, NULL, _congrats, _congrats, NULL
          invoke PostQuitMessage, 0
        @@:
     ret
endp

proc ManageInputs
        GetTime
        sub eax, [lastFrame]
        imul eax, [speed]
        sar eax, 3                          ;Не работает с высоким фпс !!! (костыль добавил (костыль добавил (костыль добавил)))
        jnz @F
         test [timeState], 1
         jnz @F
         test [input], 10_0000b
         jz .Half
          mov eax, 1
          and [input], 1101_1111b
         .Half:
         or [input], 10_0000b
        @@:
        test [input], 1b
        jz @F
           sub [x], eax
           cmp [x], WALL_X
           jg .ok1
            add [x], eax
           .ok1:
        @@:
        test [input], 10b
        jz @F
           add [x], eax
           cmp [x], NATIVE_WIDTH-WALL_X
           jl .ok2
            sub [x], eax
           .ok2:
        @@:
        test [input], 100b
        jz @F
           sub [y], eax
           cmp [y], WALL_Y
           jg .ok3
            add [y], eax
           .ok3:
        @@:
        test [input], 1000b
        jz @F
           add [y], eax
           cmp [y], NATIVE_HEIGHT-WALL_Y
           jl .ok4
            sub [y], eax
           .ok4:
        @@:
        test [input], 1_0000b
        jz @F
           GetTime
           mov ecx, [bulletCooldown]
           add ecx, BULLET_COOLDOWN
           cmp eax, ecx
           jb @F
              mov edx, [PBRPtr]
              mov [bulletCooldown], eax
              mov ecx, [x]
              mov word [arrPB + edx], cx
              mov ecx, [y]
              mov word [arrPB + edx + 2], cx
              mov dword [arrPB + edx + 4], eax
              mov byte [arrPB + edx + 8], 1
              stdcall rGet, 1
              shl eax, 1
              or byte [arrPB + edx + 8], al
              add [PBRPtr], BULLET_SIZE
              cmp [PBRPtr], BULLET_COUNT*BULLET_SIZE
              jb @F
                 sub [PBRPtr], BULLET_COUNT*BULLET_SIZE
        @@:
     ret
endp

proc DeleteBullets uses ebx
        mov ebx, [PBLPtr]
        @@:
        add ebx, BULLET_SIZE
        cmp ebx, BULLET_COUNT*BULLET_SIZE
        jb .Cool
           sub ebx, BULLET_COUNT*BULLET_SIZE
     .Cool:
        mov eax, [PBLPtr]
        cmp eax, [PBRPtr]
        ja .Inverse
        cmp ebx, [PBRPtr]
        jae .Skip
        jmp .Done
     .Inverse:
        cmp ebx, [PBLPtr]
        ja  .Done
        cmp ebx, [PBRPtr]
        jb .Done
        jmp .Skip
     .Done:
        GetTime
        sub eax, dword [arrPB + ebx + 4]
        sal eax, BULLET_SPEED_SAL
        add ax, word [arrPB + ebx]
        cmp ax, 2000
        jbe .Skip
        add [PBLPtr], BULLET_SIZE
        cmp [PBLPtr], BULLET_COUNT*BULLET_SIZE
        jb @B
            sub [PBLPtr], BULLET_COUNT*BULLET_SIZE
        jmp @B
     .Skip:
     ret
endp

proc DrawBullets uses ebx
       mov ebx, [PBLPtr]
      .Start:
        add ebx, BULLET_SIZE
        cmp ebx, BULLET_COUNT*BULLET_SIZE
        jb .Cool
           sub ebx, BULLET_COUNT*BULLET_SIZE
     .Cool:
        mov eax, [PBLPtr]
        cmp eax, [PBRPtr]
        ja .Inverse
        cmp ebx, [PBRPtr]
        jae .Skip
        jmp .Done
     .Inverse:
        cmp ebx, [PBLPtr]
        ja  .Done
        cmp ebx, [PBRPtr]
        jb .Done
        jmp .Skip
     .Done:
        test byte [arrPB + ebx + 8], 1
        jz .Start
        GetTime
        sub eax, dword [arrPB + ebx + 4]
        sal eax, BULLET_SPEED_SAL
        add ax, word [arrPB + ebx]
        xor ecx, ecx
        mov cx, word [arrPB + ebx + 2]

        ;Хитбоксы

        push eax ecx

        sub eax, [ex]
        cmp eax, 0
        jg @F
        neg eax
        @@:
        cmp eax, BULLET_HB_W+ENEMY_HB_W
        ja .Nohit
        sub ecx, [ey]
        cmp ecx, 0
        jg @F
        neg ecx
        @@:
        cmp ecx, BULLET_HB_H+ENEMY_HB_H
        ja .Nohit
        mov [arrPB + ebx + 8], 0
        mov eax, [bulletDamage]
        sub [EHP], eax

        pop ecx eax
        jmp .Start
        .Nohit:

        test byte [arrPB + ebx + 8], 10b
        jz .z
        invoke SelectObject, [hdcImg], [One]
        jmp @F
        .z:
        invoke SelectObject, [hdcImg], [Zero]
        @@:

        pop ecx eax

        sub eax, NUM_WIDTH/2
        sub ecx, NUM_HEIGHT/2

        invoke TransparentBlt, [hdcBuf], eax, ecx, NUM_WIDTH, NUM_HEIGHT, [hdcImg], 0, 0, NUM_WIDTH, NUM_HEIGHT, 0x00FF00FF

        jmp .Start

      .Skip:
        ret
endp

proc DrawEnemy
     invoke SelectObject, [hdcImg], [OnoshkoImage]

     cmp [aniTime], 0
     jz .no

     GetTrueTime
     sub eax, [startTime]
     cmp eax, [aniTime]
     jae .fin

     mov [ftemp], eax
     fild [ftemp]
     fidiv [aniTime]
     mov [ftemp], 1
     fisub [ftemp]

     fld st0
     fmulp

     fchs
     fiadd [ftemp]
     fld st0
     mov eax, [ney]
     sub eax, [sey]
     mov [ftemp], eax
     fimul [ftemp]
     fiadd [sey]
     fistp [ftemp]
     mov ecx, [ftemp]
     mov eax, [nex]
     sub eax, [sex]
     mov [ftemp], eax
     fimul [ftemp]
     fiadd [sex]
     fistp [ftemp]
     mov eax, [ftemp]
     mov [ex], eax
     mov [ey], ecx
     jmp .yes
     .fin:
     push [nex]
     pop [ex]
     push [ney]
     pop [ey]
     mov [aniTime], 0

     mov eax, [OnoshkoIdle]
     mov [OnoshkoImage], eax

     .no:
     mov eax, [ex]
     mov ecx, [ey]
     .yes:
     sub eax, ENEMY_WIDTH/2
     sub ecx, ENEMY_HEIGHT/2
     invoke TransparentBlt, [hdcBuf], eax, ecx, ENEMY_WIDTH, ENEMY_HEIGHT, [hdcImg], 0, 0, ONOSHKO_WIDTH, ONOSHKO_HEIGHT, 0x00FF00FF
     ret
endp

proc DrawBullet, bx, by

     mov edx, [bx]
     mov ecx, [by]
     push edx ecx

     sub edx, [x]
     cmp edx, 0
     jg @F
      neg edx
     @@:
     cmp edx, PLAYER_HB_W+EBULL_HB_W
     ja .Nohit
      sub ecx, [y]
      cmp ecx, 0
      jg @F
       neg ecx
      @@:
      cmp ecx, PLAYER_HB_H+EBULL_HB_H
      ja .Nohit

       call TakeDamage

     .Nohit:

     pop ecx edx

     sub edx, REDBULL_WIDTH/2
     sub ecx, REDBULL_HEIGHT/2

     invoke BitBlt, [hdcBuf], edx, ecx, REDBULL_WIDTH, REDBULL_HEIGHT, [hdcImg], 0, 0, SRCPAINT
     ret
endp

proc TakeDamage
     GetTime
     sub eax, [hitTime]
     sub eax, INV_TIME
     jl .NoHit
      comcall [pDsbDeath], IDirectSoundBuffer, SetCurrentPosition, 0
      comcall [pDsbDeath], IDirectSoundBuffer, Play, 0, 0, 0
      dec [HP]
      jnz @F
       invoke PostQuitMessage, 0
       jmp .NoHit
      @@:
       Call Clear
       GetTime
       mov [hitTime], eax
       push [y]
       pop [expy]
       push [x]
       pop [expx]
     .NoHit:
     ret
endp

proc DrawPulse, img, x, y, t, w, h, p, f, d, iw, ih, m
     invoke SelectObject, [hdcImg], [img]
     GetTime
     sub eax, [t]
     cmp eax, [d]
     ja .Skip
      xchg eax, ebx
      cmp ebx, [p]
      ja .After
       xor edx, edx
       mov eax, [w]
       mul ebx
       div [p]
       xchg eax, ecx

       jmp .General
      .After:
       push ebx
       sub ebx, [p]
       xor edx, edx
       mov eax, [w]
       mul ebx
       div [f]
       mov ecx, [w]
       sub ecx, eax
       pop ebx

      .General:

      xor edx, edx
      mov eax, [h]
      mul ebx
      div [d]

      mov edx, [x]
      sub edx, ecx
      sal ecx, 1

      mov ebx, [y]
      sub ebx, eax
      sal eax, 1

      invoke StretchBlt, [hdcBuf], edx, ebx, ecx, eax, [hdcImg], 0, 0, [iw], [ih], [m]
     .Skip:
     ret
endp

proc SetPulse, img, x, y
      push [x]
      pop [pulseX]
      push [y]
      pop [pulseY]
      GetTime
      mov [pulseT], eax
      push [img]
      pop [pulseImg]
     ret
endp

proc DrawSplash
     imul eax, [currAttack], sizeof.Attack
     add eax, Attacks + Attack.nameLen
     cmp dword [eax], 0
     je .Skip
      invoke SelectObject, [hdcImg], [Splash]
      GetTime
      sub eax, [nextAttack]
      cmp eax, SPLASH_DURATION
      ja .Skip
       mov [ftemp], eax
       fild [ftemp]
       mov [ftemp], SPLASH_DIVISOR
       fidiv [ftemp]
       mov [ftemp], FSPLASH_TOFF
       fsub [ftemp]
       fptan
       fistp [ftemp]
       mov [ftemp], SPLASH_MULTIPLE
       fimul [ftemp]
       mov [ftemp], SPLASH_OFF
       fiadd [ftemp]
       fistp [ftemp]
       mov eax, [ftemp]
       push eax
       invoke TransparentBlt, [hdcBuf], eax, SPLASH_Y, SPLASH_WIDTH, SPLASH_HEIGHT, [hdcImg], 0, 0, ART_WIDTH, ART_HEIGHT, 0x00FF00FF
       imul eax, [currAttack], sizeof.Attack
       add eax, Attacks
       pop ebx
       add ebx, 50
       invoke TextOut, [hdcBuf], ebx, SPLASH_Y+SPLASH_HEIGHT - 70, [eax], [eax + 4]
     .Skip:
     ret
endp

proc ChangeAttack
     inc [currAttack]
     call Clear
     cmp [currAttack], LAST_ATTACK
     ja .End
     je .Last
     cmp [threshold], 0
     jne @F
      mov [threshold], SC_THRESHOLD
      mov [bulletDamage], BULLET_DAMAGE
      mov [EHP], ENEMY_HEALTH
      dec [_hb]
      GetTime
      mov [eexpT], eax
      push [ex]
      pop [eexpX]
      push [ey]
      pop [eexpY]
      comcall [pDsbDeath], IDirectSoundBuffer, SetCurrentPosition, 0
      comcall [pDsbDeath], IDirectSoundBuffer, Play, 0, 0, 0
      jmp .Fin
     .Last:
      mov [threshold], 0
      mov [bulletDamage], LAST_BULLET_DAMAGE
      mov [EHP], ENEMY_HEALTH
      dec [_hb]
      comcall [pDsbSpellcard], IDirectSoundBuffer, SetCurrentPosition, 0
      comcall [pDsbSpellcard], IDirectSoundBuffer, Play, 0, 0, 0
      jmp .Fin
     @@:
      mov [threshold], 0
      mov [bulletDamage], SC_BULLET_DAMAGE
      xor eax, eax
      mov al, byte [_hb]
      sub al, '0'+1
      stdcall SetPulse, [Delphi + eax*4], [ex], [ey]
      comcall [pDsbSpellcard], IDirectSoundBuffer, SetCurrentPosition, 0
      comcall [pDsbSpellcard], IDirectSoundBuffer, Play, 0, 0, 0
     .Fin:
     GetTime
     mov [nextAttack], eax
     imul eax, [currAttack], sizeof.Attack
     add eax, Attacks + Attack.iniProc
     call dword [eax]
     jmp .ret
     .End:
      ;Взрыв, аплодисменты, ура и т.д.
      mov [threshold], -10000
      comcall [pDsbCheers], IDirectSoundBuffer, Play, 0, 0, 0
      GetTime
      add eax, END_DELAY
      mov [endTimer], eax
     .ret:
     ret
endp

proc TimeLock
     GetTime
     or [timeState], 1b
     mov [timeSet], eax
     ret
endp

proc TimeUnlock
     and [timeState], 1111_1110b
     GetTime
     sub eax, [timeSet]
     add [timeDif], eax
     ret
endp

proc fGetTime
     test [timeState], 1b
     jnz .Stop
      invoke TimeGetTime
      sub eax, [timeDif]
      jmp .ret
     .Stop:
      mov eax, [timeSet]
     .ret:
     ret
endp

proc drawBg
     locals
      i db ?
      j db ?
      k db ?
     endl
     invoke SelectObject, [hdcImg], [bgOne]
     xor edi, edi
     GetTime
     sub eax, [bgTime]
     sar eax, BG_SAR
     cmp eax, NUM_WIDTH*BG_MARGIN
     jl @F
      sub [bgStart], BG_WIDTH
      GetTime
      mov [bgTime], eax
      xor eax, eax
     @@:
     mov esi, eax
     sub esi, NUM_WIDTH*BG_MARGIN
     mov ebx, [bgStart]
     mov [k], BG_HEIGHT
     .k:
      mov [j], BG_WIDTH
      .j:
       mov [i], 8
       mov al, [ebx]
       .i:
        test al, 1
        push eax
        jne @F
         invoke SelectObject, [hdcImg], [bgOne]
         jmp .Skip
        @@:
         invoke SelectObject, [hdcImg], [bgZero]
        .Skip:
        invoke StretchBlt, [hdcBuf], edi, esi, NUM_HEIGHT*BG_SCALE, NUM_WIDTH*BG_SCALE, [hdcImg], 0,0, NUM_HEIGHT, NUM_WIDTH, SRCCOPY
        add edi, NUM_HEIGHT*BG_MARGIN
        pop eax
        ror al, 1
        dec [i]
       jnz .i
       inc ebx
       dec [j]
      jnz .j
      add esi, NUM_WIDTH*BG_MARGIN
      xor edi, edi
      dec [k]
     jnz .k
     ret
endp

proc Clear
     mov eax, arrEB
     mov ecx, EBULLET_COUNT
     @@:
      and byte [eax + 4], 1111_1110b
      add eax, EBULLET_SIZE
     loop @B
     mov [shotNumber], 0
     ret
endp

proc IntToStr, pStr
     locals
      ten dd 10
     endl
     xor ecx, ecx
     @@:
      xor edx, edx
      div [ten]
      push edx
      inc ecx
      test eax, eax
     jnz @B
     mov ebx, [pStr]
     @@:
      pop edx
      add dx, '0'
      mov word [ebx + eax], dx
      add eax, 2
     loop @B
     sar eax, 1
     ret
endp

proc Meax uses eax ecx edx
        locals
           _debug TCHAR 8 dup (?),0
        endl
        mov ecx, 8
.Loop:
        push eax
        and ax, $F
        cmp ax, 9
        jbe @F
        add ax, 'A'-'0'-10
@@:
        add ax, '0'
        mov [_debug-sizeof.TCHAR+ecx*sizeof.TCHAR],ax
        pop eax
        ror eax, 4
        loop .Loop
        lea ecx, [_debug]
        invoke MessageBox, NULL, ecx, 0, 0
        ret
endp

proc rIni
     GetTime
     mov [rPrev], eax
     ret
endp

proc rGet uses edx, max

     mov        eax, [rPrev]
     mov        edx, 8405h
     imul       eax, edx
     add        eax, 1
     mov        [rPrev], eax

     mov        edx, [max]
     inc        edx
     mul        edx

     xchg eax, edx
     ret
endp

nullProc:
        ret

include 'Attacks.asm'

section '.data' data readable writeable

  _class TCHAR 'PotatoVSOnoshko',0
  _title TCHAR 'Картошка vs. Оношко',0
  _error TCHAR 'Startup failed.',0

  _atkSO TCHAR 'Лекция ~ Неопределённое поведение'
  latkSO = ($ - _atkSO)/sizeof.TCHAR
  _atkLL TCHAR 'Лекция ~ 42'
  latkLL = ($ - _atkLL)/sizeof.TCHAR
  _atkTS TCHAR 'Лекция  ~ Остановка в развитии'
  latkTS = ($ - _atkTS)/sizeof.TCHAR
  _atkFin TCHAR 'Последний экзамен ~ Конец эпохи'
  latkFin = ($ - _atkFin)/sizeof.TCHAR

  _potato TCHAR 'data\potato.bmp',0
  _onoshkoidle TCHAR 'data\onoshkoidle.bmp',0
  _onoshkomove TCHAR 'data\onoshkomove.bmp',0
  _onoshkoshoot TCHAR 'data\onoshkoshoot.bmp',0
  _icon TCHAR 'data\potato.ico',0
  _bgzero TCHAR 'data\bgzero.bmp',0
  _bgone TCHAR 'data\bgone.bmp',0
  _zero TCHAR 'data\zero.bmp',0
  _one TCHAR 'data\one.bmp',0
  _redbull TCHAR 'data\redbull.bmp',0
  _greenbull TCHAR 'data\greenbull.bmp',0
  _bluebull TCHAR 'data\bluebull.bmp',0
  _explosion TCHAR 'data\explosion.bmp',0
  _splash TCHAR 'data\splash.bmp',0
  _C TCHAR 'data\C.bmp',0
  _delphi TCHAR 'data\delphi.bmp',0
  _fasm TCHAR 'data\fasm.bmp',0
  _shoot du 'data\shoot.raw', 0
  _death du 'data\death.raw', 0
  _timestop du 'data\timestop.raw',0
  _spellcard du 'data\spellcard.raw',0
  _cheers du 'data\cheers.raw',0
  _voice du 'data\voice.raw',0
  _so TCHAR 'Stack Overflow',0
  _congrats TCHAR 'Поздравляю!!!',0

  _hb du ENEMY_HB

  wc WNDCLASS 0,WindowProc,0,0,NULL,NULL,NULL,0,NULL,_class

  Attacks:
  Attack NULL, 0, ini, genRC, drawRC, DefaultMove
  Attack _atkSO, latkSO, ini, genSO, drawSO, LockMove
  Attack NULL, 0, ini, genGS, drawGS, LockMove
  Attack _atkLL, latkLL, iniLL, genLL, drawLL, LLMove
  Attack NULL, 0, ini, genBB, drawBB, LockMove
  Attack _atkTS, latkTS, iniTS, genTS, drawTS, TSMove
  Attack _atkFin, latkFin, ini, genFin, drawFin, LockMove
  Attack NULL, 0, nullProc, nullProc, nullProc, LockMove

  x dd 100
  y dd 540

  ex dd 1500
  ey dd 540

  input db 0

  EHP dd ENEMY_HEALTH
  MEHP dd ENEMY_HEALTH

  HP dd PLAYER_HP

  fullRect RECT 0, 0, NATIVE_WIDTH, NATIVE_HEIGHT

  speed dd BASE_SPEED

  PBLPtr dd -BULLET_SIZE
  PBRPtr dd 0

  aniTime dd 0

  degToRad dd 180

  circleNumber dw 0
  bulletNumber dw 0
  shotNumber db 0

  currAttack dd 0

  dsbdesc DSBUFFERDESC sizeof.DSBUFFERDESC, DSBCAPS_STATIC, 0xFFFFF, 0, wfx, 0, 0

  wfx WAVEFORMATEX WAVE_FORMAT_PCM, 1, 22050, 44100, 2, 16, 0

  pulseImg dd 0
  timeState db 0
  timeDif dd 0

  bulletDamage dd BULLET_DAMAGE
  threshold dd SC_THRESHOLD

  bgStart dd arrEB

  _fps du 5 dup (?)

  lastAttack dd ?
  lastShot dd ?

  OnoshkoImage dd ?

  fpsLen dd ?
  hFile dd ?

  sex dd ?
  sey dd ?

  nex dd ?
  ney dd ?

  startTime dd ?

  expx dd ?
  expy dd ?

  msg MSG

  ps PAINTSTRUCT

  hdc dd ?
  hdcImg dd ?
  hdcBuf dd ?
  bmMem dd ?

  RedBrush dd ?
  GrayBrush dd ?
  DGrayBrush dd ?
  Brush dd ?
  Potato dd ?
  OnoshkoIdle dd ?
  OnoshkoMove dd ?
  OnoshkoShoot dd ?
  Zero dd ?
  One dd ?
  bgZero dd ?
  bgOne dd ?
  RedBull dd ?
  GreenBull dd ?
  BlueBull dd ?
  Icon dd ?
  Font dd ?
  Explosion dd ?
  Splash dd ?
  Delphi dd ?
  fasm dd ?
  C dd ?

  lastFrame dd ?
  bulletCooldown dd ?
  hitTime dd ?
  nextAttack dd ?
  bgTime dd ?

  ftemp dd ?

  rPrev dd ?

  lastSecond dd ?
  fpsCounter dd ?

  pulseX dd ?
  pulseY dd ?
  pulseT dd ?

  eexpX dd ?
  eexpY dd ?
  eexpT dd ?

  timeSet dd ?

  pDSound dd ?
  pDsbDeath dd ?
  pDsbShoot dd ?
  pDsbZaWarudo dd ?
  pDsbSpellcard dd ?
  pDsbCheers dd ?
  pDsbVoice dd ?
  pb1 dd ?
  lb1 dd ?
  pb2 dd ?
  lb2 dd ?

  endTimer dd ?

  arrPB db BULLET_COUNT * BULLET_SIZE dup (?)   ;xxyyttttd
  arrEB db EBULLET_COUNT * EBULLET_SIZE dup (?) ;ttttdxxyy334455667

section '.idata' import data readable writeable

  library kernel32,'KERNEL32.DLL',\
          user32,'USER32.DLL',\
          gdi32, 'GDI32.DLL',\
          winmm,'WINMM.dll',\
          msimg32, 'MSIMG32.DLL',\
          dsound, 'DSOUND.DLL'

  import winmm, TimeGetTime, 'timeGetTime'

  import msimg32, TransparentBlt, 'TransparentBlt'

  import dsound, DirectSoundCreate, 'DirectSoundCreate'

  include 'api\kernel32.inc'
  include 'api\user32.inc'
  include 'api\gdi32.inc'