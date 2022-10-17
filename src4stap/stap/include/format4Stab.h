2000  format (///,' ',20a4,///,                                         &
          ' c o n t r o l   i n f o r m a t i o n',//,                  &
          '      number of nodal points',10(' .'),' (numnp)  = ',i5,//, &
          '      number of element groups',9(' .'),' (numeg)  = ',i5,//,&
          '      number of load cases',11(' .'),' (nlcase) = ',i5,//,   &
          '      solution mode ',14(' .'),' (modex)  = ',i5,/,          &
          '         eq.0, data check',/,                                &
          '         eq.1, execution')

2010  format (////,'     load case number',7(' .'),' = ',i5,//,         &
              '     number of concentrated loads . = ',i5)


2025  format (//,' total system data',///,                              &
             '     number of equations',14(' .'),'(neq) = ',i5,//,      &
             '     number of matrix elements',11(' .'),'(nwk) = ',i5,//,&
             '     maximum half bandwidth ',12(' .'),'(mk ) = ',i5,//,  &
             '     mean half bandwidth',14(' .'),'(mm ) = ',i5)
2030  format (//,' s o l u t i o n   t i m e   l o g   i n   s e c',//, &
       '            for problem',//,' ',20a4,///,                       &
       '     time for input phase ',14(' .'),' =',e12.2,//,             &
       '     time for calculation of stiffness matrix  . . . . =',e12.2,&
       //,                                                              &
       '     time for factorization of stiffness matrix  . . . =',e12.2,&
       //,                                                              &
       '     time for load case solutions ',10(' .'),' =',e12.2,///,    &
       '      t o t a l   s o l u t i o n   t i m e  . . . . . =',e12.2)

