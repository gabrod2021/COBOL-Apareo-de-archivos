       01 WS-SALIDA-APAREO.

          05 WS-SAL-APA-SEPARADOR           PIC X(133) VALUES ALL "-".

          05 WS-SAL-APA-TITULOS.
             10 FILLER                      PIC X(11)
                                                VALUE 'ID-EMPLEADO'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(25) VALUE 'NOMBRE'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(25) VALUE 'APELLIDO'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(50)
                                                VALUE 'DIRECCION'.
             10 FILLER                      PIC X(03) VALUES ' | '.
             10 FILLER                      PIC X(10)
                                                VALUE 'COD-POSTAL'.

          05 WS-SAL-APA-DETALLE.
             10 WS-SAL-APA-ID-EMPLEADO      PIC 9(08).
             10 FILLER                      PIC X(06) VALUE '    | '.
             10 WS-SAL-APA-NOMBRE           PIC X(25).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-APA-APELLIDO         PIC X(25).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-APA-DIRECCION        PIC X(50).
             10 FILLER                      PIC X(09) VALUE ' |       '.
             10 WS-SAL-APA-COD-POSTAL       PIC 9(04).
