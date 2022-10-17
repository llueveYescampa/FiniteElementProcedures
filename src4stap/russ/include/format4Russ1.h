 2000 format (' e l e m e n t   d e f i n i t i o n',///,               &
              ' element type ',13(' .'),'( npar(1) ) . . =',i5,/,       &
              '     eq.1, truss elements',/,                            &
              '     eq.2, elements currently',/,                        &
              '     eq.3, not available',//,                            &
              ' number of elements.',10(' .'),'( npar(2) ) . . =',i5,//)

 2010 format (' m a t e r i a l   d e f i n i t i o n',///,             &
              ' number of different sets of material',/,                &
              ' and cross-sectional  constants ',                       &
                                 4(' .'),'( npar(3) ) . . =',i5,//)
 2020 format ('  set       young''s     cross-sectional',/,             &
              ' number     modulus',10x,'area',/,                       &
              15x,'e',14x,'a')

 2040 format (//,' e l e m e n t   i n f o r m a t i o n',///,         &
              ' element     node     node       material',/,           &
              ' number-n      i        j       set number',/)

