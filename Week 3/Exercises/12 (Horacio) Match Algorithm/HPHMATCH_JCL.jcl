//HPHMATCH JOB 1,NOTIFY=&SYSUID                                                 
//***************************************************/                          
//* Copyright Contributors to the COBOL Programming Course                      
//* SPDX-License-Identifier: CC-BY-4.0                                          
//***************************************************/                          
//COBRUN  EXEC IGYWCL                                                           
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HPHMATCH),DISP=SHR                           
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HPHMATCH),DISP=SHR                          
//***************************************************/                          
// IF RC = 0 THEN                                                               
//***************************************************/                          
//RUN     EXEC PGM=HPHMATCH                                                     
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR                                       
//ENTRADA1  DD DSN=&SYSUID..DATA.MATCH.A1,DISP=SHR                              
//ENTRADB2  DD DSN=&SYSUID..DATA.MATCH.B2,DISP=SHR                              
//SALIDA1   DD SYSOUT=*,OUTLIM=15000                                            
//SYSOUT    DD SYSOUT=*,OUTLIM=15000                                            
//CEEDUMP   DD DUMMY                                                            
//SYSUDUMP  DD DUMMY                                                            
//***************************************************/                          
// ELSE                                                                         
// ENDIF                                                                        
