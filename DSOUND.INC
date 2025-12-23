DSSCL_NORMAL =              0x00000001
DSSCL_PRIORITY =            0x00000002
DSSCL_EXCLUSIVE =           0x00000003
DSSCL_WRITEPRIMARY =        0x00000004

DSBCAPS_PRIMARYBUFFER =     0x00000001
DSBCAPS_STATIC =            0x00000002

WAVE_FORMAT_PCM =           0x0001

struct WAVEFORMATEX
  wFormatTag dw ?
  nChannels dw ?
  nSamplesPerSec dd ?
  nAvgBytesPerSec dd ?
  nBlockAlign dw ?
  wBitsPerSample dw ?
  cbSize dw ?
ends

struct DSBUFFERDESC
    dwSize dd sizeof.DSBUFFERDESC
    dwFlags dd ?
    dwBufferBytes dd ?
    dwReserved dd 0
    lpwfxFormat dd ?
    guid3DAlgorithm dq ?
    guid2 dq ?
ends

interface IDirectSound,\
          QueryInterface,\
          AddRef,\
          Release,\
          CreateSoundBuffer,\
          GetCaps,\
          DuplicateSoundBuffer,\
          SetCooperativeLevel,\
          Compact,\
          GetSpeakerConfig,\
          SetSpeakerConfig,\
          Initialize

interface IDirectSoundBuffer,\
          QueryInterface,\
          AddRef,\
          Release,\
          GetCaps,\
          GetCurrentPosition,\
          GetFormat,\
          GetVolume,\
          GetPan,\
          GetFrequency,\
          GetStatus,\
          Initialize,\
          Lock,\
          Play,\
          SetCurrentPosition,\
          SetFormat,\
          SetVolume,\
          SetPan,\
          SetFrequency,\
          Stop,\
          Unlock,\
          Restore