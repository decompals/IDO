#if !defined(__gls_h_)
#define __gls_h_

/*
** Copyright 1995-2095, Silicon Graphics, Inc.
** All Rights Reserved.
** 
** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
** the contents of this file may not be disclosed to third parties, copied or
** duplicated in any form, in whole or in part, without the prior written
** permission of Silicon Graphics, Inc.
** 
** RESTRICTED RIGHTS LEGEND:
** Use, duplication or disclosure by the Government is subject to restrictions
** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
** rights reserved under the Copyright Laws of the United States.
*/

#define GLS_LINKAGE

#include <GL/gl.h>
#include <stddef.h>
#include <stdio.h>

#if defined(__cplusplus)
    extern "C" {
#endif /* defined(__cplusplus) */

typedef void GLSchar;
typedef GLint GLSenum;
typedef long long GLSlong;
typedef GLint GLSopcode;

typedef void (*GLScaptureEntryFunc)(
    GLSopcode inOpcode, const GLvoid *optinParam1
);
typedef void (*GLScaptureExitFunc)(
    GLSopcode inOpcode, const GLvoid *optinParam1, GLvoid *optinoutReturn
);

typedef size_t (*GLSreadFunc)(size_t inCount, GLvoid *outBuf);
typedef size_t (*GLSwriteFunc)(size_t inCount, const GLvoid *inBuf);

#if defined(__cplusplus)
    typedef void (*GLSfunc)(...);
#else /* !defined(__cplusplus) */
    typedef void (*GLSfunc)();
#endif /* defined(__cplusplus) */

#if !defined(GL_VERSION_1_1)
    #define GL_V2F                                0x2A20
    #define GL_V3F                                0x2A21
    #define GL_C4UB_V2F                           0x2A22
    #define GL_C4UB_V3F                           0x2A23
    #define GL_C3F_V3F                            0x2A24
    #define GL_N3F_V3F                            0x2A25
    #define GL_C4F_N3F_V3F                        0x2A26
    #define GL_T2F_V3F                            0x2A27
    #define GL_T4F_V4F                            0x2A28
    #define GL_T2F_C4UB_V3F                       0x2A29
    #define GL_T2F_C3F_V3F                        0x2A2A
    #define GL_T2F_N3F_V3F                        0x2A2B
    #define GL_T2F_C4F_N3F_V3F                    0x2A2C
    #define GL_T4F_C4F_N3F_V4F                    0x2A2D
#endif /* !defined(GL_VERSION_1_1) */

/*************************************************************/

/* CaptureFlags */
/*      GLS_NONE */
#define GLS_CAPTURE_EXECUTE_BIT                   0x00000001
#define GLS_CAPTURE_WRITE_BIT                     0x00000002

/* ImageFlags */
/*      GLS_NONE */
#define GLS_IMAGE_NULL_BIT                        0x00000001

/* WriteFlags */
/*      GLS_NONE */
#define GLS_WRITE_APPEND_BIT                      0x00000001

/* Fundamental */
#define GLS_NONE                                  0x0000

/* AbortMode */
/*      GLS_NONE */
#define GLS_ALL                                   0x0010
#define GLS_LAST                                  0x0011

/* API */
#define GLS_API_GLS                               0x0020
#define GLS_API_GL                                0x0021

/* BlockType */
#define GLS_FRAME                                 0x0030
#define GLS_HEADER                                0x0031
#define GLS_INIT                                  0x0032
#define GLS_STATIC                                0x0033

/* Boolean */
#define GLS_CAPTURE_VERTEX_ARRAYS                 0x0040

/* CaptureStreamType */
#define GLS_CONTEXT                               0x0050
#define GLS_BINARY_LSB_FIRST                      0x0051
#define GLS_BINARY_MSB_FIRST                      0x0052
#define GLS_TEXT                                  0x0053

/* ChannelTarget */
#define GLS_DEFAULT_READ_CHANNEL                  0x0060
#define GLS_DEFAULT_WRITE_CHANNEL                 0x0061

/* CopyStreamType */
/*      GLS_NONE */
/*      GLS_CONTEXT */
/*      GLS_BINARY_LSB_FIRST */
/*      GLS_BINARY_MSB_FIRST */
/*      GLS_TEXT */

/* ErrorCode */
/*      GLS_NONE */
#define GLS_DECODE_ERROR                          0x0070
#define GLS_IO_ERROR                              0x0071
#define GLS_PARAMETER_ERROR                       0x0072
#define GLS_RESOURCE_ERROR                        0x0073
#define GLS_STATE_ERROR                           0x0074
#define GLS_UNSUPPORTED_COMMAND                   0x0075
#define GLS_UNSUPPORTED_EXTENSION                 0x0076
#define GLS_UNSUPPORTED_VERSION                   0x0077

/* ExternStreamType */
/*      GLS_BINARY_LSB_FIRST */
/*      GLS_BINARY_MSB_FIRST */
/*      GLS_TEXT */

/* GetCommandi */
#define GLS_API                                   0x0080
#define GLS_IS_CLIENT_GET_COMMAND                 0x0081
#define GLS_IS_CLIENT_STATE_COMMAND               0x0082
#define GLS_IS_GET_COMMAND                        0x0083

/* GetFunc */
#define GLS_CAPTURE_ENTRY_FUNC                    0x0090
#define GLS_CAPTURE_EXIT_FUNC                     0x0091
#define GLS_READ_FUNC                             0x0092
#define GLS_WRITE_FUNC                            0x0093

/* GetListc */
#define GLS_CONTEXT_STREAM_LIST                   0x00A0
#define GLS_READ_PREFIX_LIST                      0x00A1

/* GetListi */
#define GLS_OUT_ARG_SIZE_LIST                     0x00B0

/* GetListl */
#define GLS_OUT_ARG_POINTER_LIST                  0x00C0

/* GetPointer */
/*      GLS_DEFAULT_READ_CHANNEL */
/*      GLS_DEFAULT_WRITE_CHANNEL */
#define GLS_DATA_POINTER                          0x00D0

/* GetStreamType */
/*      GLS_NONE */
/*      GLS_CONTEXT */
/*      GLS_BINARY_LSB_FIRST */
/*      GLS_BINARY_MSB_FIRST */
/*      GLS_TEXT */
#define GLS_UNKNOWN                               0x00E0

/* GetStreamc */
#define GLS_READ_NAME                             0x00F0
#define GLS_WRITE_NAME                            0x00F1

/* GetStreami */
#define GLS_CRC32                                 0x0100
#define GLS_STREAM_TYPE                           0x0101

/* Getc */
#define GLS_EXTENSIONS                            0x0110
#define GLS_RELEASE                               0x0111
#define GLS_VENDOR                                0x0112
#define GLS_WRITE_PREFIX                          0x0113

/* Geti */
#define GLS_ABORT_MODE                            0x0120
#define GLS_BLOCK_TYPE                            0x0121
#define GLS_CALL_NESTING                          0x0122
#define GLS_CAPTURE_NESTING                       0x0123
#define GLS_CONTEXT_STREAM_COUNT                  0x0124
#define GLS_MAX_CALL_NESTING                      0x0125
#define GLS_MAX_CAPTURE_NESTING                   0x0126
#define GLS_OUT_ARG_COUNT                         0x0127
#define GLS_READ_PREFIX_COUNT                     0x0128
#define GLS_STREAM_VERSION_MAJOR                  0x0129
#define GLS_STREAM_VERSION_MINOR                  0x012A
#define GLS_STRING_TYPE                           0x012B
#define GLS_VERSION_MAJOR                         0x012C
#define GLS_VERSION_MINOR                         0x012D

/* Getiv */
#define GLS_CURRENT_TIME                          0x0140

/* Headerc */
#define GLS_AUTHOR                                0x0150
#define GLS_TITLE                                 0x0151
#define GLS_VERSION                               0x0152

/* Headerf */
#define GLS_ASPECT                                0x0160
#define GLS_CONTRAST                              0x0161

/* Headerfv */
#define GLS_GAMMA                                 0x0170
#define GLS_RED_POINT                             0x0171
#define GLS_GREEN_POINT                           0x0172
#define GLS_BLUE_POINT                            0x0173
#define GLS_WHITE_POINT                           0x0174

/* Headeri */
#define GLS_ACCUM_RED_BITS                        0x0180
#define GLS_ACCUM_GREEN_BITS                      0x0181
#define GLS_ACCUM_BLUE_BITS                       0x0182
#define GLS_ACCUM_ALPHA_BITS                      0x0183
#define GLS_AUX_BUFFERS                           0x0184
#define GLS_COLOR_RED_BITS                        0x0185
#define GLS_COLOR_GREEN_BITS                      0x0186
#define GLS_COLOR_BLUE_BITS                       0x0187
#define GLS_COLOR_ALPHA_BITS                      0x0188
#define GLS_DEPTH_BITS                            0x0189
#define GLS_DOUBLEBUFFER                          0x018A
#define GLS_HEIGHT                                0x018B
#define GLS_STENCIL_BITS                          0x018C
#define GLS_STEREO                                0x018D
#define GLS_TILEABLE                              0x018E
/*      GLS_SAMPLES_SGIS */

/* Headeriv */
#define GLS_CREATE_TIME                           0x01A0

/* StringType */
#define GLS_UCS1                                  0x01B0
#define GLS_UCS2                                  0x01B1
#define GLS_UCS4                                  0x01B2

/* GL_SGIS_multisample */
#define GLS_SAMPLES_SGIS                          0x0200

/* GLS opcodes */
#define GLS_OP_glsBeginGLS                        16
#define GLS_OP_glsBlock                           17
#define GLS_OP_glsCallStream                      18
#define GLS_OP_glsDrawVertexArray                 19
#define GLS_OP_glsEndGLS                          20
#define GLS_OP_glsError                           21
#define GLS_OP_glsHeaderc                         22
#define GLS_OP_glsHeaderf                         23
#define GLS_OP_glsHeaderfv                        24
#define GLS_OP_glsHeaderi                         25
#define GLS_OP_glsHeaderiv                        26
#define GLS_OP_glsRequireExtension                27
#define GLS_OP_glsRequireVersion                  28
#define GLS_OP_glsUnsupportedCommand              29
#define GLS_OP_glsBeginObj                        30
#define GLS_OP_glsComment                         31
#define GLS_OP_glsEndObj                          32
#define GLS_OP_glsNumbv                           33
#define GLS_OP_glsNumd                            34
#define GLS_OP_glsNumdv                           35
#define GLS_OP_glsNumf                            36
#define GLS_OP_glsNumfv                           37
#define GLS_OP_glsNumi                            38
#define GLS_OP_glsNumiv                           39
#define GLS_OP_glsNuml                            40
#define GLS_OP_glsNumlv                           41
#define GLS_OP_glsNumsv                           42
#define GLS_OP_glsPad                             43
#define GLS_OP_glsString                          44
#define GLS_OP_glsSwapBuffers                     45

/* GL opcodes */
#define GLS_OP_glAccum                            191
#define GLS_OP_glAlphaFunc                        214
#define GLS_OP_glAreTexturesResident              300
#define GLS_OP_glAreTexturesResidentEXT           65502
#define GLS_OP_glArrayElement                     280
#define GLS_OP_glArrayElementEXT                  65493
#define GLS_OP_glBegin                            55
#define GLS_OP_glBindTexture                      301
#define GLS_OP_glBindTextureEXT                   65503
#define GLS_OP_glBitmap                           56
#define GLS_OP_glBlendColorEXT                    65520
#define GLS_OP_glBlendEquationEXT                 65521
#define GLS_OP_glBlendFunc                        215
#define GLS_OP_glCallList                         50
#define GLS_OP_glCallLists                        51
#define GLS_OP_glClear                            181
#define GLS_OP_glClearAccum                       182
#define GLS_OP_glClearColor                       184
#define GLS_OP_glClearDepth                       186
#define GLS_OP_glClearIndex                       183
#define GLS_OP_glClearStencil                     185
#define GLS_OP_glClipPlane                        128
#define GLS_OP_glColor3bv                         57
#define GLS_OP_glColor3dv                         58
#define GLS_OP_glColor3fv                         59
#define GLS_OP_glColor3iv                         60
#define GLS_OP_glColor3sv                         61
#define GLS_OP_glColor3ubv                        62
#define GLS_OP_glColor3uiv                        63
#define GLS_OP_glColor3usv                        64
#define GLS_OP_glColor4bv                         65
#define GLS_OP_glColor4dv                         66
#define GLS_OP_glColor4fv                         67
#define GLS_OP_glColor4iv                         68
#define GLS_OP_glColor4sv                         69
#define GLS_OP_glColor4ubv                        70
#define GLS_OP_glColor4uiv                        71
#define GLS_OP_glColor4usv                        72
#define GLS_OP_glColorMask                        188
#define GLS_OP_glColorMaterial                    129
#define GLS_OP_glColorPointer                     281
#define GLS_OP_glColorPointerEXT                  65494
#define GLS_OP_glColorTableParameterfvSGI         65477
#define GLS_OP_glColorTableParameterivSGI         65478
#define GLS_OP_glColorTableSGI                    65476
#define GLS_OP_glConvolutionFilter1DEXT           65528
#define GLS_OP_glConvolutionFilter2DEXT           65529
#define GLS_OP_glConvolutionParameterfEXT         65530
#define GLS_OP_glConvolutionParameterfvEXT        65531
#define GLS_OP_glConvolutionParameteriEXT         65532
#define GLS_OP_glConvolutionParameterivEXT        65533
#define GLS_OP_glCopyColorTableSGI                65479
#define GLS_OP_glCopyConvolutionFilter1DEXT       65534
#define GLS_OP_glCopyConvolutionFilter2DEXT       65535
#define GLS_OP_glCopyPixels                       229
#define GLS_OP_glCopyTexImage1D                   294
#define GLS_OP_glCopyTexImage1DEXT                65483
#define GLS_OP_glCopyTexImage2D                   295
#define GLS_OP_glCopyTexImage2DEXT                65484
#define GLS_OP_glCopyTexSubImage1D                296
#define GLS_OP_glCopyTexSubImage1DEXT             65485
#define GLS_OP_glCopyTexSubImage2D                297
#define GLS_OP_glCopyTexSubImage2DEXT             65486
#define GLS_OP_glCopyTexSubImage3DEXT             65487
#define GLS_OP_glCullFace                         130
#define GLS_OP_glDeformSGIX                       65415
#define GLS_OP_glDeformationMap3dSGIX             65413
#define GLS_OP_glDeformationMap3fSGIX             65414
#define GLS_OP_glDeleteLists                      52
#define GLS_OP_glDeleteTextures                   302
#define GLS_OP_glDeleteTexturesEXT                65472
#define GLS_OP_glDepthFunc                        219
#define GLS_OP_glDepthMask                        189
#define GLS_OP_glDepthRange                       262
#define GLS_OP_glDetailTexFuncSGIS                65489
#define GLS_OP_glDisable                          192
#define GLS_OP_glDisableClientState               282
#define GLS_OP_glDrawArrays                       283
#define GLS_OP_glDrawArraysEXT                    65495
#define GLS_OP_glDrawBuffer                       180
#define GLS_OP_glDrawElements                     284
#define GLS_OP_glDrawPixels                       231
#define GLS_OP_glEdgeFlagPointer                  285
#define GLS_OP_glEdgeFlagPointerEXT               65496
#define GLS_OP_glEdgeFlagv                        73
#define GLS_OP_glEnable                           193
#define GLS_OP_glEnableClientState                286
#define GLS_OP_glEnd                              74
#define GLS_OP_glEndList                          49
#define GLS_OP_glEvalCoord1dv                     206
#define GLS_OP_glEvalCoord1fv                     207
#define GLS_OP_glEvalCoord2dv                     208
#define GLS_OP_glEvalCoord2fv                     209
#define GLS_OP_glEvalMesh1                        210
#define GLS_OP_glEvalMesh2                        212
#define GLS_OP_glEvalPoint1                       211
#define GLS_OP_glEvalPoint2                       213
#define GLS_OP_glFeedbackBuffer                   172
#define GLS_OP_glFinish                           194
#define GLS_OP_glFlush                            195
#define GLS_OP_glFlushRasterSGIX                  65409
#define GLS_OP_glFogFuncSGIS                      65467
#define GLS_OP_glFogf                             131
#define GLS_OP_glFogfv                            132
#define GLS_OP_glFogi                             133
#define GLS_OP_glFogiv                            134
#define GLS_OP_glFrameZoomSGIX                    65411
#define GLS_OP_glFrontFace                        135
#define GLS_OP_glFrustum                          263
#define GLS_OP_glGenLists                         53
#define GLS_OP_glGenTextures                      303
#define GLS_OP_glGenTexturesEXT                   65473
#define GLS_OP_glGetBooleanv                      232
#define GLS_OP_glGetClipPlane                     233
#define GLS_OP_glGetColorTableParameterfvSGI      65481
#define GLS_OP_glGetColorTableParameterivSGI      65482
#define GLS_OP_glGetColorTableSGI                 65480
#define GLS_OP_glGetConvolutionFilterEXT          65504
#define GLS_OP_glGetConvolutionParameterfvEXT     65505
#define GLS_OP_glGetConvolutionParameterivEXT     65506
#define GLS_OP_glGetDetailTexFuncSGIS             65490
#define GLS_OP_glGetDoublev                       234
#define GLS_OP_glGetError                         235
#define GLS_OP_glGetFloatv                        236
#define GLS_OP_glGetHistogramEXT                  65509
#define GLS_OP_glGetHistogramParameterfvEXT       65510
#define GLS_OP_glGetHistogramParameterivEXT       65511
#define GLS_OP_glGetInstrumentsSGIX               65468
#define GLS_OP_glGetIntegerv                      237
#define GLS_OP_glGetLightfv                       238
#define GLS_OP_glGetLightiv                       239
#define GLS_OP_glGetMapdv                         240
#define GLS_OP_glGetMapfv                         241
#define GLS_OP_glGetMapiv                         242
#define GLS_OP_glGetMaterialfv                    243
#define GLS_OP_glGetMaterialiv                    244
#define GLS_OP_glGetMinmaxEXT                     65512
#define GLS_OP_glGetMinmaxParameterfvEXT          65513
#define GLS_OP_glGetMinmaxParameterivEXT          65514
#define GLS_OP_glGetPixelMapfv                    245
#define GLS_OP_glGetPixelMapuiv                   246
#define GLS_OP_glGetPixelMapusv                   247
#define GLS_OP_glGetPointerv                      287
#define GLS_OP_glGetPointervEXT                   65497
#define GLS_OP_glGetPolygonStipple                248
#define GLS_OP_glGetSeparableFilterEXT            65507
#define GLS_OP_glGetSharpenTexFuncSGIS            65492
#define GLS_OP_glGetString                        249
#define GLS_OP_glGetTexEnvfv                      250
#define GLS_OP_glGetTexEnviv                      251
#define GLS_OP_glGetTexFilterFuncSGIS             65463
#define GLS_OP_glGetTexGendv                      252
#define GLS_OP_glGetTexGenfv                      253
#define GLS_OP_glGetTexGeniv                      254
#define GLS_OP_glGetTexImage                      255
#define GLS_OP_glGetTexLevelParameterfv           258
#define GLS_OP_glGetTexLevelParameteriv           259
#define GLS_OP_glGetTexParameterfv                256
#define GLS_OP_glGetTexParameteriv                257
#define GLS_OP_glHint                             136
#define GLS_OP_glHistogramEXT                     65515
#define GLS_OP_glIglooInterfaceSGIX               65412
#define GLS_OP_glIndexMask                        190
#define GLS_OP_glIndexPointer                     288
#define GLS_OP_glIndexPointerEXT                  65498
#define GLS_OP_glIndexdv                          75
#define GLS_OP_glIndexfv                          76
#define GLS_OP_glIndexiv                          77
#define GLS_OP_glIndexsv                          78
#define GLS_OP_glIndexubv                         306
#define GLS_OP_glInitNames                        175
#define GLS_OP_glInterleavedArrays                289
#define GLS_OP_glIsEnabled                        260
#define GLS_OP_glIsList                           261
#define GLS_OP_glIsTexture                        304
#define GLS_OP_glIsTextureEXT                     65474
#define GLS_OP_glLightModelf                      141
#define GLS_OP_glLightModelfv                     142
#define GLS_OP_glLightModeli                      143
#define GLS_OP_glLightModeliv                     144
#define GLS_OP_glLightf                           137
#define GLS_OP_glLightfv                          138
#define GLS_OP_glLighti                           139
#define GLS_OP_glLightiv                          140
#define GLS_OP_glLineStipple                      145
#define GLS_OP_glLineWidth                        146
#define GLS_OP_glListBase                         54
#define GLS_OP_glLoadIdentity                     264
#define GLS_OP_glLoadIdentityDeformationMapSGIX   65416
#define GLS_OP_glLoadMatrixd                      266
#define GLS_OP_glLoadMatrixf                      265
#define GLS_OP_glLoadName                         176
#define GLS_OP_glLogicOp                          216
#define GLS_OP_glMap1d                            198
#define GLS_OP_glMap1f                            199
#define GLS_OP_glMap2d                            200
#define GLS_OP_glMap2f                            201
#define GLS_OP_glMapGrid1d                        202
#define GLS_OP_glMapGrid1f                        203
#define GLS_OP_glMapGrid2d                        204
#define GLS_OP_glMapGrid2f                        205
#define GLS_OP_glMaterialf                        147
#define GLS_OP_glMaterialfv                       148
#define GLS_OP_glMateriali                        149
#define GLS_OP_glMaterialiv                       150
#define GLS_OP_glMatrixMode                       267
#define GLS_OP_glMinmaxEXT                        65516
#define GLS_OP_glMultMatrixd                      269
#define GLS_OP_glMultMatrixf                      268
#define GLS_OP_glNewList                          48
#define GLS_OP_glNormal3bv                        79
#define GLS_OP_glNormal3dv                        80
#define GLS_OP_glNormal3fv                        81
#define GLS_OP_glNormal3iv                        82
#define GLS_OP_glNormal3sv                        83
#define GLS_OP_glNormalPointer                    290
#define GLS_OP_glNormalPointerEXT                 65499
#define GLS_OP_glOrtho                            270
#define GLS_OP_glPassThrough                      177
#define GLS_OP_glPipelineInstrumentsBufferSGIX    65469
#define GLS_OP_glPixelMapfv                       225
#define GLS_OP_glPixelMapuiv                      226
#define GLS_OP_glPixelMapusv                      227
#define GLS_OP_glPixelStoref                      223
#define GLS_OP_glPixelStorei                      224
#define GLS_OP_glPixelTexGenSGIX                  65458
#define GLS_OP_glPixelTransferf                   221
#define GLS_OP_glPixelTransferi                   222
#define GLS_OP_glPixelZoom                        220
#define GLS_OP_glPointParameterfSGIS              65465
#define GLS_OP_glPointParameterfvSGIS             65466
#define GLS_OP_glPointSize                        151
#define GLS_OP_glPollInstrumentsSGIX              65470
#define GLS_OP_glPolygonMode                      152
#define GLS_OP_glPolygonOffset                    293
#define GLS_OP_glPolygonOffsetEXT                 65522
#define GLS_OP_glPolygonStipple                   153
#define GLS_OP_glPopAttrib                        196
#define GLS_OP_glPopClientAttrib                  307
#define GLS_OP_glPopMatrix                        271
#define GLS_OP_glPopName                          178
#define GLS_OP_glPrioritizeTextures               305
#define GLS_OP_glPrioritizeTexturesEXT            65475
#define GLS_OP_glPushAttrib                       197
#define GLS_OP_glPushClientAttrib                 308
#define GLS_OP_glPushMatrix                       272
#define GLS_OP_glPushName                         179
#define GLS_OP_glRasterPos2dv                     84
#define GLS_OP_glRasterPos2fv                     85
#define GLS_OP_glRasterPos2iv                     86
#define GLS_OP_glRasterPos2sv                     87
#define GLS_OP_glRasterPos3dv                     88
#define GLS_OP_glRasterPos3fv                     89
#define GLS_OP_glRasterPos3iv                     90
#define GLS_OP_glRasterPos3sv                     91
#define GLS_OP_glRasterPos4dv                     92
#define GLS_OP_glRasterPos4fv                     93
#define GLS_OP_glRasterPos4iv                     94
#define GLS_OP_glRasterPos4sv                     95
#define GLS_OP_glReadBuffer                       228
#define GLS_OP_glReadPixels                       230
#define GLS_OP_glRectdv                           96
#define GLS_OP_glRectfv                           97
#define GLS_OP_glRectiv                           98
#define GLS_OP_glRectsv                           99
#define GLS_OP_glReferencePlaneSGIX               65410
#define GLS_OP_glRenderMode                       174
#define GLS_OP_glResetHistogramEXT                65517
#define GLS_OP_glResetMinmaxEXT                   65518
#define GLS_OP_glRotated                          273
#define GLS_OP_glRotatef                          274
#define GLS_OP_glSampleMaskSGIS                   65525
#define GLS_OP_glSamplePatternSGIS                65526
#define GLS_OP_glScaled                           275
#define GLS_OP_glScalef                           276
#define GLS_OP_glScissor                          154
#define GLS_OP_glSelectBuffer                     173
#define GLS_OP_glSeparableFilter2DEXT             65508
#define GLS_OP_glShadeModel                       155
#define GLS_OP_glSharpenTexFuncSGIS               65491
#define GLS_OP_glSpriteParameterfSGIX             65459
#define GLS_OP_glSpriteParameterfvSGIX            65460
#define GLS_OP_glSpriteParameteriSGIX             65461
#define GLS_OP_glSpriteParameterivSGIX            65462
#define GLS_OP_glStartInstrumentsSGIX             65471
#define GLS_OP_glStencilFunc                      217
#define GLS_OP_glStencilMask                      187
#define GLS_OP_glStencilOp                        218
#define GLS_OP_glStopInstrumentsSGIX              65408
#define GLS_OP_glTagSampleBufferSGIX              65527
#define GLS_OP_glTexCoord1dv                      100
#define GLS_OP_glTexCoord1fv                      101
#define GLS_OP_glTexCoord1iv                      102
#define GLS_OP_glTexCoord1sv                      103
#define GLS_OP_glTexCoord2dv                      104
#define GLS_OP_glTexCoord2fv                      105
#define GLS_OP_glTexCoord2iv                      106
#define GLS_OP_glTexCoord2sv                      107
#define GLS_OP_glTexCoord3dv                      108
#define GLS_OP_glTexCoord3fv                      109
#define GLS_OP_glTexCoord3iv                      110
#define GLS_OP_glTexCoord3sv                      111
#define GLS_OP_glTexCoord4dv                      112
#define GLS_OP_glTexCoord4fv                      113
#define GLS_OP_glTexCoord4iv                      114
#define GLS_OP_glTexCoord4sv                      115
#define GLS_OP_glTexCoordPointer                  291
#define GLS_OP_glTexCoordPointerEXT               65500
#define GLS_OP_glTexEnvf                          162
#define GLS_OP_glTexEnvfv                         163
#define GLS_OP_glTexEnvi                          164
#define GLS_OP_glTexEnviv                         165
#define GLS_OP_glTexFilterFuncSGIS                65464
#define GLS_OP_glTexGend                          166
#define GLS_OP_glTexGendv                         167
#define GLS_OP_glTexGenf                          168
#define GLS_OP_glTexGenfv                         169
#define GLS_OP_glTexGeni                          170
#define GLS_OP_glTexGeniv                         171
#define GLS_OP_glTexImage1D                       160
#define GLS_OP_glTexImage2D                       161
#define GLS_OP_glTexImage3DEXT                    65519
#define GLS_OP_glTexImage4DSGIS                   65456
#define GLS_OP_glTexParameterf                    156
#define GLS_OP_glTexParameterfv                   157
#define GLS_OP_glTexParameteri                    158
#define GLS_OP_glTexParameteriv                   159
#define GLS_OP_glTexSubImage1D                    298
#define GLS_OP_glTexSubImage1DEXT                 65523
#define GLS_OP_glTexSubImage2D                    299
#define GLS_OP_glTexSubImage2DEXT                 65524
#define GLS_OP_glTexSubImage3DEXT                 65488
#define GLS_OP_glTexSubImage4DSGIS                65457
#define GLS_OP_glTranslated                       277
#define GLS_OP_glTranslatef                       278
#define GLS_OP_glVertex2dv                        116
#define GLS_OP_glVertex2fv                        117
#define GLS_OP_glVertex2iv                        118
#define GLS_OP_glVertex2sv                        119
#define GLS_OP_glVertex3dv                        120
#define GLS_OP_glVertex3fv                        121
#define GLS_OP_glVertex3iv                        122
#define GLS_OP_glVertex3sv                        123
#define GLS_OP_glVertex4dv                        124
#define GLS_OP_glVertex4fv                        125
#define GLS_OP_glVertex4iv                        126
#define GLS_OP_glVertex4sv                        127
#define GLS_OP_glVertexPointer                    292
#define GLS_OP_glVertexPointerEXT                 65501
#define GLS_OP_glViewport                         279

/*************************************************************/

/* GLS global commands */
extern void glsContext (GLint inContext);
extern void glsDeleteContext (GLint inContext);
extern GLint glsGenContext (void);
extern GLint* glsGetAllContexts (void);
extern GLSenum glsGetBinaryType (GLint inSwapCount);
extern GLint glsGetCommandi (GLSopcode inOpcode, GLSenum inAttrib);
extern GLint glsGetCurrentContext (void);
extern GLSenum glsGetError (void);
extern GLSfunc glsGetNullCommandFunc (GLSopcode inOpcode);
extern GLint glsGetOpcodeCount (GLSenum inAPI);
extern const GLSopcode* glsGetOpcodes (GLSenum inAPI);
extern GLboolean glsIsClientState (GLSenum inAPI, GLSenum inAttrib);
extern GLboolean glsIsContext (GLint inContext);

/* GLS immediate commands */
extern void glsAbortCall (GLSenum inMode);
extern void glsAppendReadPrefix (const GLSchar *inPrefix);
extern GLboolean glsBeginCapture (const GLSchar *inStreamName, GLSenum inCaptureStreamType, GLint inWriteFlags);
extern void glsCallArray (GLSenum inExternStreamType, size_t inCount, const GLvoid *inArray);
extern void glsCaptureEntryFunc (GLScaptureEntryFunc inFunc);
extern void glsCaptureExitFunc (GLScaptureExitFunc inFunc);
extern void glsCaptureFlags (GLSopcode inOpcode, GLint inFlags);
extern void glsChannel (GLSenum inTarget, FILE *inChannel);
extern void glsCommandFunc (GLSopcode inOpcode, GLSfunc inFunc);
extern GLSenum glsCopyStream (const GLSchar *inSource, const GLSchar *inDest, GLSenum inDestType, GLint inWriteFlags);
extern void glsDataPointer (GLvoid *inPointer);
extern void glsDeleteStream (const GLSchar *inName);
extern void glsDisable (GLSenum inAttrib);
extern void glsEnable (GLSenum inAttrib);
extern void glsEndCapture (void);
extern void glsFlush (void);
extern GLint glsGetCaptureFlags (GLSopcode inOpcode);
extern GLSfunc glsGetCommandFunc (GLSopcode inOpcode);
extern const GLSchar* glsGetCommandName (GLSopcode inOpcode);
extern GLSopcode glsGetCommandOpcode (const GLSchar *inName);
extern const GLSchar* glsGetEnumName (GLSenum inAPI, GLSenum inValue);
extern GLint glsGetEnumValue (GLSenum inAPI, const GLSchar *inName);
extern GLSfunc glsGetFunc (GLSenum inAttrib);
extern const GLSchar* glsGetHeaderc (GLSenum inAttrib);
extern GLfloat glsGetHeaderf (GLSenum inAttrib);
extern GLfloat* glsGetHeaderfv (GLSenum inAttrib, GLfloat *outVec);
extern GLint glsGetHeaderi (GLSenum inAttrib);
extern GLint* glsGetHeaderiv (GLSenum inAttrib, GLint *outVec);
extern const GLSchar* glsGetListc (GLSenum inAttrib, GLint inIndex);
extern GLint glsGetListi (GLSenum inAttrib, GLint inIndex);
extern GLSlong glsGetListl (GLSenum inAttrib, GLint inIndex);
extern GLvoid* glsGetPointer (GLSenum inAttrib);
extern size_t glsGetStreamSize (const GLSchar *inName);
extern const GLSchar* glsGetStreamc (GLSenum inAttrib, const GLSchar *inName);
extern GLint glsGetStreami (GLSenum inAttrib, const GLSchar *inName);
extern const GLSchar* glsGetc (GLSenum inAttrib);
extern GLint glsGeti (GLSenum inAttrib);
extern GLint* glsGetiv (GLSenum inAttrib, GLint *outVec);
extern GLboolean glsIsContextStream (const GLSchar *inName);
extern GLboolean glsIsEnabled (GLSenum inAttrib);
extern GLboolean glsIsExtensionSupported (const GLSchar *inExtension);
extern GLboolean glsIsVersionSupported (GLSenum inAPI, GLint inVersionMajor, GLint inVersionMinor);
extern void glsPrependReadPrefix (const GLSchar *inPrefix);
extern void glsReadFunc (GLSreadFunc inFunc);
extern void glsRemoveReadPrefix (GLint inIndex);
extern void glsResetAlignment (void);
extern void glsStringType (GLSenum inStringType);
extern void glsWriteFunc (GLSwriteFunc inFunc);
extern void glsWritePrefix (const GLSchar *inPrefix);

/* GLS encodable commands */
extern void glsBeginGLS (GLint inVersionMajor, GLint inVersionMinor);
extern void glsBlock (GLSenum inBlockType);
extern GLSenum glsCallStream (const GLSchar *inName);
extern void glsDrawVertexArray (GLenum inMode, GLenum inFormat, GLint inCount, const GLvoid *inArray);
extern void glsEndGLS (void);
extern void glsError (GLSopcode inOpcode, GLSenum inError);
extern void glsHeaderc (GLSenum inAttrib, const GLSchar *inString);
extern void glsHeaderf (GLSenum inAttrib, GLfloat inVal);
extern void glsHeaderfv (GLSenum inAttrib, const GLfloat *inVec);
extern void glsHeaderi (GLSenum inAttrib, GLint inVal);
extern void glsHeaderiv (GLSenum inAttrib, const GLint *inVec);
extern void glsRequireExtension (const GLSchar *inExtension);
extern void glsRequireVersion (GLSenum inAPI, GLint inVersionMajor, GLint inVersionMinor);
extern void glsUnsupportedCommand (const GLSchar *inCommand);

/* GLS encodable-nop commands */
extern void glsBeginObj (const GLSchar *inTag);
extern void glsComment (const GLSchar *inComment);
extern void glsEndObj (void);
extern void glsNumbv (const GLSchar *inTag, GLint inCount, const GLbyte *inVec);
extern void glsNumd (const GLSchar *inTag, GLdouble inVal);
extern void glsNumdv (const GLSchar *inTag, GLint inCount, const GLdouble *inVec);
extern void glsNumf (const GLSchar *inTag, GLfloat inVal);
extern void glsNumfv (const GLSchar *inTag, GLint inCount, const GLfloat *inVec);
extern void glsNumi (const GLSchar *inTag, GLint inVal);
extern void glsNumiv (const GLSchar *inTag, GLint inCount, const GLint *inVec);
extern void glsNuml (const GLSchar *inTag, GLSlong inVal);
extern void glsNumlv (const GLSchar *inTag, GLint inCount, const GLSlong *inVec);
extern void glsNumsv (const GLSchar *inTag, GLint inCount, const GLshort *inVec);
extern void glsPad (void);
extern void glsString (const GLSchar *inTag, const GLSchar *inString);
extern void glsSwapBuffers (void);

#if defined(__cplusplus)
    }
#endif /* defined(__cplusplus) */

#endif /* defined(__gls_h_) */
