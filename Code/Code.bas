'--------------------------------------------------------'
' 														 '
' Configurable 4 to 20 mA Comperator with AVR atmega 8	 '
'														 '		
'--------------------------------------------------------'


$regfile = "m8def.dat"
$crystal = 4000000

'-------------------------------------------
'---------------Config LCD----------------
'-------------------------------------------
Config Lcd = 16 * 2
Config Lcdpin = Pin , Db4 = Portb.3 , Db5 = Portb.2 , Db6 = Portb.1 , Db7 = Portb.0 , E = Portb.4 , Rs = Portb.5
Cursor Off Noblink

'-------------------------------------------
'---------------Config ports----------------
'-------------------------------------------
Config Portb.6 = Output
Config Portd.7 = Output
Config Portd.4 = Input
Config Portd.3 = Input
Config Portd.2 = Input
Config Portd.1 = Input
Config Portd.0 = Input

'-------------------------------------------
'---------------Config ADC------------------
'-------------------------------------------
Config Adc = Single , Prescaler = Auto

Config Watchdog = 2048
Start Watchdog

'-------------------------------------------
'--------------Config Interrupts------------
'-------------------------------------------
Config Int0 = Rising
Enable Interrupts
On Int0 Intsub
Enable Int0


'-------------------------------------------
'---------------Config Timers---------------
'-------------------------------------------
Config Timer1 = Timer , Prescale = 256
Enable Timer1
On Timer1 Isr_von_timer1
Enable Interrupts
Timer1 = 34285

'-------------------------------------------
'---------------define variables------------
'-------------------------------------------
Dim Iinput1 As Word , Iinput2 As Word
Dim Page As Byte , Bitchange As Bit
Dim Curr_curr As Single , Curr_time As Word
Dim Eram_curr As Eram Single , Eram_time As Eram Word
Dim Eram_preset As Eram Byte , Preset As Byte
Dim Ii1 As Single , Ii2 As Single , Def As Single
Dim Second As Word , Alert As Bit , Alertt As Bit
Dim Lcd_timer As Byte , Lcdbit As Bit
Dim Z1 As String * 4 , Z2 As String * 4 , Zdef As String * 4


'-------------------------------------------
'---------------preset Value----------------
'-------------------------------------------
Page = 1 : Bitchange = 1
Preset = Eram_preset
If Preset = 255 Then
   Eram_curr = 0.5
   Eram_time = 30
   Eram_preset = 10
End If

Second = 0 : Alert = 0 : Alertt = 0
Set Portd.7 : Set Lcdbit

'-------------------------------------------
'---------------Main Body-------------------
'-------------------------------------------
Do
Iinput1 = Getadc(5)
Waitms 100
Iinput2 = Getadc(3)

Ii1 = Iinput1 / 48
Ii2 = Iinput2 / 48
Def = Ii1 - Ii2
Def = Abs(def)


If Def > Curr_curr And Page = 1 Then
   Set Alert
   Set Portd.7 : Set Lcdbit
Else
  ' If Alertt = 0 Then
Gosub Intsub

   'End If
End If




If Page = 1 And Bitchange = 1 Then
Curr_curr = Eram_curr
Curr_time = Eram_time
Bitchange = 0


End If


'  Time Delay Page
If Page = 2 And Bitchange = 1 Then

Cls : Lcd "Set Delay Time "
Locate 2 , 1 : Lcd Curr_time : Lcd "  Sec  "
Bitchange = 0
End If



'  Current Difference Page
If Page = 3 And Bitchange = 1 Then

Cls : Lcd "Current Diff"
Locate 2 , 1 : Lcd Curr_curr : Lcd "  mAmp    "
Bitchange = 0
End If



If Pind.4 = 0 Then
Set Portd.7 : Set Lcdbit : Lcd_timer = 0
'Cls : Lcd "INC"
   If Page = 2 Then
       If Curr_time < 271 Then
          Curr_time = Curr_time + 30
       End If
   End If
   If Page = 3 Then
       If Curr_curr < 16 Then
          Curr_curr = Curr_curr + 0.5
       End If
   End If
Waitms 240
Bitchange = 1
End If

If Pind.3 = 0 Then
Set Portd.7 : Set Lcdbit : Lcd_timer = 0
'Cls : Lcd "DEC"

   If Page = 2 Then
       If Curr_time > 30 Then
          Curr_time = Curr_time - 30
       End If
   End If
   If Page = 3 Then
       If Curr_curr > 0.8 Then
          Curr_curr = Curr_curr - 0.5
       End If
   End If
Bitchange = 1
Waitms 240

End If


'  OK Button
If Pind.2 = 0 Then
Set Portd.7 : Set Lcdbit : Lcd_timer = 0

   If Page = 2 Then
     Eram_time = Curr_time
   End If

   If Page = 3 Then
     Eram_curr = Curr_curr
   End If

End If



'  Time delay Button
If Pind.1 = 0 And Alert = 0 Then
Set Portd.7 : Set Lcdbit : Lcd_timer = 0
Curr_time = Eram_time
   If Page = 2 Then
      Page = 1
   Else
      Page = 2
   End If
Waitms 240
Bitchange = 1
End If



'  Current Diff Button
If Pind.0 = 0 And Alert = 0 Then
Set Portd.7 : Set Lcdbit : Lcd_timer = 0
Curr_curr = Eram_curr
   If Page = 3 Then
      Page = 1
   Else
      Page = 3
   End If
Waitms 240
Bitchange = 1

End If

Reset Watchdog

Loop



' timer subroutine
Isr_von_timer1:

Timer1 = 34285
If Page = 1 And Bitchange = 0 And Alert = 0 Then
   Cls : Waitms 30 : Cls
   Z1 = Fusing(ii1 , "#.#")
   Z2 = Fusing(ii2 , "#.#")
   'Locate 1 , 1 : Lcd Iinput1
   'Locate 2 , 1 : Lcd Iinput2
   Locate 1 , 1 : Lcd "Input1: " : Lcd Z1 : Lcd " mA"
   Locate 2 , 1 : Lcd "Input2: " : Lcd Z2 : Lcd " mA"
End If


If Alert = 1 Then
   Incr Second
   Cls : Lcd " mA  Difference "
   Zdef = Fusing(def , "##.##")

   Locate 2 , 6 : Lcd Zdef
   ' Locate 2 , 1 : Lcd Zdef :
   If Second > Curr_time Then
      Set Portb.6 : Set Alertt
   End If



End If

If Lcdbit = 1 Then
      Incr Lcd_timer

      If Lcd_timer > 30 And Alertt = 0 Then
         Reset Portd.7
         Reset Lcdbit
         Lcd_timer = 0
         Page = 1 : Bitchange = 1
      End If

End If


Return



Intsub:

   Reset Portb.6
   Reset Alertt : Reset Alert
   Second = 0

Return