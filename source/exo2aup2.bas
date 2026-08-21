print : print " EXO >> AUP2 parser"
#include once "Afx\AfxStr.inc"
#include once "Afx\CFindFile.inc"
#include once "string.bi"
dim shared as string a,b,c,d,e,ee,f(),h,r,rx,pp,tv,rsf,pm
dim shared as string scene_array(0 To 49) 'scene limit for aup projects
dim shared as CWSTR tm
dim shared as integer j,k,m,n,g,w,fc,jj,kk
dim shared as boolean vv,na
dim shared as double fr
'const pm = "C:\Decoy\Video\Migration\"



Function PurifySceneName (ByRef raw_str As String) As String
    'High-utility string purification engine
    Dim cleaned As String : Dim ch As UByte
    Dim As Integer valid_char_count, i
    For i = 1 To Len(raw_str)
        ch = Asc(raw_str, i)
        'Keep: standard Western alphanumerics, spaces, hyphens, and underscores
        If (ch > 64 And ch < 91) Or (ch > 96 And ch < 123) Or (ch > 47 And ch < 58) Or ch = 32 Or ch = 45 Or ch = 95 Then
            cleaned &= Chr(ch) : valid_char_count += 1
        Else
            Exit For 'Cut string when striking trailing binary flags
        End If
    Next
    If valid_char_count < 3 Then Return ""
    Return Trim(cleaned)
End Function



Sub ExtractAupScenes (ByRef aup_path As String)
    If Open(aup_path For Binary Access Read As #1) <> 0 Then
        Print "Error opening .aup project file." : Exit Sub
    End If
    'Limitation: will not detect scenes if their default names are unchanged.
    erase scene_array
    Dim As UByte cb,sn
    Dim As String string_accumulator, parsed_token
    Dim As Boolean active_recording = False
    dim scene_count As Integer
    'alternative method for finding scene 1 and beyond
    seek #1,1
    do until eof(1) 'find 80EEn marker
        get #1,,cb : if cb <> 56 then continue do
        get #1,,cb : if cb <> 48 then continue do
        get #1,,cb : if cb <> 69 then continue do
        get #1,,cb : if cb <> 69 then continue do
        get #1,,cb : if cb <> 110 then continue do
        j = seek(1) : print " Start marker found at location";j : exit do
    loop
    do until eof(1) 'find Tween marker
        get #1,,cb : if cb <> 84 then continue do
        get #1,,cb : if cb <> 119 then continue do
        get #1,,cb : if cb <> 101 then continue do
        get #1,,cb : if cb <> 101 then continue do
        get #1,,cb : if cb <> 110 then continue do
        k = seek(1) : print " End marker found at location";k : exit do
    loop
    seek #1,j
    do until eof(1) 'find ISSMs
        if seek(1) > k then exit do
        get #1,,cb : if cb <> 241 and cb <> 242 then continue do
        get #1,,cb : if cb <> 0 then continue do
        'Hex F1 00 or F2 00 signature found
        get #1,,cb : if cb < 5 then continue do 'cb is either 5 or 8
        get #1,,sn 'scene number
        if cb = 5 then scene_array(sn) = "Scene " & str(sn) : continue do
        'length of scene name is 1 or greater
        'Individual Scene Structural Matrix is 8 bytes
        if sn = 1 then m = seek(1) - 3 'remember start of the first ISSM
        seek #1,seek(1) + 7 'jump to the first letter of the scene name
        g = cb - 8 : d = "" : for n = 1 to g : get #1,,cb : d = d & chr(cb) : next
        scene_array(sn) = d : scene_count += 1 : If scene_count = 49 Then Exit do 'scene limit reached
    loop
    seek #1,j
    'find root scene
    While Not EOF(1)
        Get #1,,cb
        if seek(1) > m then exit while
        'Build text data structures sequentially
        If (cb >= 32 And cb <= 126) Or (cb >= 128 And cb <= 254) Then
            string_accumulator &= Chr(cb) : continue while
        End if
        If Len(string_accumulator) >= 3 Then
            parsed_token = PurifySceneName(string_accumulator)
            scene_array(0) = parsed_token
        End If
        string_accumulator = ""
    Wend
    Close #1
    d = aup_path : k = scene_count + 1 : fc = fc + 1
    print tab(7);fc;". ";d;": ";
    select case scene_count
        case 0 : print "No scenes were found."
        case 1 : print "1 scene was found. Project is oomplete."
        case else : print "This project contains";k;" scenes." : vv = true
    end select
    'For n = 0 To 50 : Print n;". ";scene_array(n) : Next
End Sub



/'
' Function to safely flip BBGGRR hex tokens into standard RRGGBB
Function FormatExoColorToAup2(ByRef exo_color As String) As String
    Dim As String clean_hex = Trim(exo_color)
    If Len(clean_hex) < 6 Then Return "ffffff" ' Fallback white
    Dim As String rr = Right(clean_hex, 2)
    Dim As String gg = Mid(clean_hex, 3, 2)
    Dim As String bb = Left(clean_hex, 2)
    Return rr & gg & bb
End Function
'/



'Helper function to swap Big-Endian 32-bit integers to Little-Endian (x86/x64)
Function SwapEndian32(ByVal value As ULong) As ULong
  Return ((value And &h000000FF) Shl 24) Or ((value And &h0000FF00) Shl 8) Or ((value And &h00FF0000) Shr 8) Or ((value And &hFF000000) Shr 24)
End Function



Function GetNativeMp4Framerate(ByRef file_path As String) As Double
    Dim As Integer file_num = FreeFile()
    Dim As ULong raw_size = 0, atom_size = 0
    Dim As String * 4 atom_name = "    "
    Dim As ULong media_timescale = 0
    Dim As ULong frame_delta = 0
    Dim As ULong total_samples = 0
    Dim As Double media_duration = 0.0
    If Open(file_path For Binary Access Read As #file_num) <> 0 Then
        Return 29.970 ' Fallback if file cannot be opened
    End If
    ' Sequential stream parsing loop
    While Not EOF(file_num)
        Dim As ULong current_atom_start = Seek(file_num)
        Get #file_num, , raw_size
        atom_size = SwapEndian32(raw_size)
        Get #file_num, , atom_name
        ' Zero-size protection loop out
        If atom_size = 0 Then Exit While
        ' Container Atoms: Step INSIDE these blocks sequentially
        If atom_name = "moov" Or atom_name = "trak" Or atom_name = "mdia" Or atom_name = "minf" Or atom_name = "stbl" Then
            Continue While
        End If
        ' 1. FOUND MEDIA HEADER (mdhd) -> Captures clock frequency
        If atom_name = "mdhd" Then
            Dim As UByte version = 0
            Get #file_num, , version
            Seek #file_num, Seek(file_num) + 3 ' Skip flags
            If version = 1 Then
                Seek #file_num, Seek(file_num) + 16 ' Skip creation/mod times
            Else
                Seek #file_num, Seek(file_num) + 8
            End If
            Dim As ULong raw_ts = 0
            Get #file_num, , raw_ts
            media_timescale = SwapEndian32(raw_ts)
            ' Read duration field to calculate fallback time windows
            Dim As ULong raw_dur = 0
            Get #file_num, , raw_dur
            If media_timescale > 0 Then
                media_duration = CDbl(SwapEndian32(raw_dur)) / CDbl(media_timescale)
            End If
            Seek #file_num, current_atom_start + atom_size
            Continue While
        End If
        ' 2. FOUND TIME-TO-SAMPLE (stts) -> Calculates frame delta
        If atom_name = "stts" Then
            Seek #file_num, Seek(file_num) + 4 ' Skip version/flags
            Dim As ULong entry_count = 0
            Get #file_num, , entry_count
            entry_count = SwapEndian32(entry_count)
            If entry_count > 0 Then
                Dim As ULong sample_count = 0, raw_delta = 0
                Get #file_num, , sample_count 
                Get #file_num, , raw_delta    
                frame_delta = SwapEndian32(raw_delta)
            End If
            ' Validate that we have a real timescale and delta before returning
            If media_timescale > 0 And frame_delta > 0 Then
                Dim As Double calculated_fps = CDbl(media_timescale) / CDbl(frame_delta)
                ' AUDIO FILTER PROTECTION MASK:
                ' Check if the math maps exactly to common fixed hardware audio package scales:
                ' 43.0664 (44.1kHz AAC), 46.875 (48kHz AAC), 38.28125 (44.1kHz MP3), 41.6666 (48kHz MP3)
                Dim As Boolean is_audio_stream = False
                If Abs(calculated_fps - 43.06640625) < 0.0001 Then is_audio_stream = True
                If Abs(calculated_fps - 46.87500000) < 0.0001 Then is_audio_stream = True
                If Abs(calculated_fps - 38.28125000) < 0.0001 Then is_audio_stream = True
                If Abs(calculated_fps - 41.66666667) < 0.0001 Then is_audio_stream = True
                ' If it's a realistic video framerate and NOT an audio stream, process it!
                If (Not is_audio_stream) and calculated_fps >= 5.0 And calculated_fps <= 240.0  Then
                    Close #file_num
                    If Abs(calculated_fps - 29.97) < 0.05 Then Return 29.970
                    If Abs(calculated_fps - 59.94) < 0.05 Then Return 59.940
                    Return calculated_fps
                End If
            End If
            ' Reset tracking variables and continue parsing to scan for the next track block
            frame_delta = 0
            media_timescale = 0
            media_duration = 0.0
            Seek #file_num, current_atom_start + atom_size
            Continue While
        End If
        ' 3. FALLBACK: SAMPLE SIZE ATOM (stsz) -> Triggered if stts parsing gets bypassed
        If atom_name = "stsz" Then
            Seek #file_num, Seek(file_num) + 4 ' Skip version/flags
            Seek #file_num, Seek(file_num) + 4 ' Skip uniform size field
            Dim As ULong sample_count = 0
            Get #file_num, , sample_count
            total_samples = SwapEndian32(sample_count)
            If total_samples > 0 And media_duration > 0.0 Then
                Dim As Double fallback_fps = CDbl(total_samples) / media_duration
                ' Apply the same signature filter check to fallback calculations
                Dim As Boolean is_audio_fallback = False
                If Abs(fallback_fps - 43.06640625) < 0.0001 Then is_audio_fallback = True
                If Abs(fallback_fps - 46.87500000) < 0.0001 Then is_audio_fallback = True
                If (Not is_audio_fallback) and fallback_fps >= 5.0 And fallback_fps <= 240.0 Then
                    Close #file_num
                    If Abs(fallback_fps - 29.97) < 0.05 Then Return 29.970
                    If Abs(fallback_fps - 59.94) < 0.05 Then Return 59.940
                    Return fallback_fps
                End If
            End If
            ' Reset metrics for fallback layers
            total_samples = 0
        End If
        ' Skip leaf atom data cleanly using absolute coordinates
        Seek #file_num, current_atom_start + atom_size
    Wend
    Close #file_num
    Return 29.970 ' Safe fallback environment default
End Function



sub FindParameterBlock (xk as string)
  seek #7,1
  do until eof(7)
    input #7,e,j,k : if e = xk then exit do 'parameter block boundaries
  loop  
end sub



sub FindExoKey (xk as string)
  for w = jj to kk
    e = f(w,2) : if e = xk then exit for 'key index
  next w
end sub



sub GetJPNText
    'maintain UTF-8 encoding
    tm = f(j + val(c),1) : tm = CWSTR(tm,CP_UTF8) : write #3,w,tm
end sub



sub ParseExo (ef as string)
    r = "csv.dat" : fc = fc + 1 : print fc;". Input: ";ef
    if AfxFileScanA(ef,"chain=1") = 0 then
        print " No chains were found in ";ef : AfxFileCopy ef,r : goto x350
    end if
    'convert chains to csv
    n = 0 : g = 0
    open ef for input as #1
    open "chain.lst" for output as #2
    do until eof(1) = true
        line input #1,b
        if instr(b,"=") = 0 then k = val(mid(b,2)) : continue do
        if b = "chain=1" then write #2,k - 1,k + 1 : n = n + 1
    loop
    close #2
    print " Number of chains:";n
    'identify chain parameters with an exclamation point
    seek #1,1 : n = -1 : b = "EXO>AUP2: this is an intermediate data file"
    open "chain.lst" for input as #2
    open "chained.dat" for output as #3
    while eof(2) = false
        input #2,j,k
        if j < n then print #3,,"!";b else print #3,b
        'if j < n then this is a continuation of the previous chain
        'if j = n then this is the start of an adjacent chain
        if j > n then 'copy non-chain parameters
            c = "[" & str(j) & "]"
            do until b = c
                line input #1,b : print #3,b
            loop
        end if
        c = "[" & str(k) & "]"
        do until eof(1) 'prefix chain params with Space(14) + !
            line input #1,b : if b = c then exit do
            print #3,,"!";b : g = g + 1 'assuming that ,, is equivalent to Space(14)
        loop
        n = k 'remember the end index of the previous chain
    wend
    'copy the remainder of the file
    print #3,b
    do until eof(1)
        line input #1,b : print #3,b
    loop
    close #1,#2,#3
    print " Number of chain parameters:";g
    'chain to csv algorithm
    open "chained.dat" for input as #1
    open "unchained.dat" for output as #3
    print " Flattening chain data into comma separated values." : g = 0
    while eof(1) = false
        line input #1,b : if instr(b,"!") <> 15 then print #3,b : continue while
        'build frame= csv
        ee = str(val(mid(b,22))-1) 'remember !start= key value - 1
        line input #1,rx '!end= key value (to be discarded)
        w = seek(1) 'remember location of !layer= param
        while instr(b,"!") = 15
            line input #1,b
            if instr(b,"end=") > 0 then rx = b : continue while
            if instr(b,"start=") = 0 then continue while
            if instr(b,"!") <> 15 then exit while
            ee = ee & "," & str(val(mid(b,22))-1) 'join !start key values - 1
        wend
        b = "frame=" & ee & "," & str(val(mid(rx,20))-1) 'this is a frame= csv
        print #3,b : g = g + 1 'csv parameter count
x300:
        na = false : b = space(14) & "!" 'needed to initialize while/wend loop
        'begin copying chained object
        seek #1,w 'location: !layer= or movement parameter
        while instr(b,"!") = 15
            line input #1,b
            if instr(b,",") > 0 then na = true : exit while 'a movement parameter was found
            if instr(b,"=") = 0 and vv = true then rx = b : exit while 'object has been written
            print #3,b
        wend
        if na = true then
            w = seek(1) 'remember location of movement parameter
            d = left(b,instr(b,"=")) : ee = left(b,instrrev(b,","))
            'build movement csv
            while instr(b,"!") = 15
                line input #1,b
                if instr(b,d) = 0 then continue while
                m = instr(b,",") + 1
                n = instrrev(b,",") + 1
                ee = ee & mid(b,m,n-m) : rx = b
            wend
            ee = ee & mid(rx,n) : print #3,ee 'this is a movement csv
            vv = true : g = g + 1 'csv param count
            goto x300
        end if
        if vv = true then 'skip redundant data
            do while instr(b,"!") = 15
                line input #1,b
            loop
            vv = false : print #3,b
        end if
    wend
    'copy the remainder of the file
    do until eof(1)
        line input #1,b : print #3,b
    loop
    close #1,#3
    print " Number of CSV parameters:";g
    'rewrite index blocks to be sequential
    print " Sequencing index blocks."
    open "unchained.dat" for input as #1
    open r for output as #2
    line input #1,b 'discard first line (file description)
    line input #1,b : print #2,b 'write header ID
    n = -1
    while eof(1) = false
        line input #1,b
        if instr(b,"!") = 15 then b = mid(b,16) 'remove prefix
        if instr(b,"=") > 0 then print #2,b : continue while
        if instr(b,".") = 0 then n = n + 1 : d = str(n) else d = str(n) & mid(b,instr(b,"."),2)
        print #2,"[";d;"]"
    wend
    close #1,#2


x350:


    'store the mapper in an array, remove comments
    print " Initializing array." : n = 1
    open "aup2.map" for input as #1
    do until eof(1) = true
        line input #1,b : k = instr(b,chr(9)) - 1 : if k > 0 then b = left(b,k)
        f(n,1) = b : n = n + 1
    loop
    close #1


    n = AfxFileScanA(r,"param=")
    print " SemicolonSV parameters:";n
    'unpack Custom object parameters: they are stored in a semicolon chain
    open r for input as #1
    open "csv2.dat" for output as #2
    FindParameterBlock("Custom object ExoType") : vv = false
    do until eof(1)
        line input #1,a : if instr(a,"=") = 0 then print #2,a : continue do
        if a <> "_name=Custom object" then print #2,a : continue do
        g = 0
        do until eof(1)
            line input #1,a : n = instr(a,"=") : if n = 0 then exit do
            b = left(a,n - 1) : c = mid(a,n + 1) 'key=value
            select case b
                case "type" : scene_array(0) = "_name=" & f(j + val(c),1) : continue do 'rename custom object according to its type
                case "param" 'convert into INI style key values
                    c = AfxStrReplace(c,";",chr(13) & chr(10)) : c = AfxStrReplace(c,chr(34),"")
                    c = AfxStrReplace(c,"0x","") : a = rtrim(c,chr(13) & chr(10))
                case "check0","filter","name" : continue do
            end select
            g = g + 1 : scene_array(g) = a
        loop
        'write the changes
        for n = 0 to g : print #2,scene_array(n) : next : print #2,a
    loop
    close #1,#2
    'sleep:end
    AfxFileCopy "csv2.dat",r



    'mapping algorithm
    w = 0 : h = AfxGetFileName(ef) & ".aup2"
    open r for input as #1
    open "exo2aup2.log" for append as #2
    print #2,"Unmapped parameters for ";pm;ef
    open "exo2aup2.lst" for output encoding "utf-8" as #3
    open "redirect.lst" for output as #4
    open "scene linker.lst" for output as #5
    'read exo file
    do until eof(1) = true
        line input #1,a : n = instr(a,"=")
        if n = 0 then write #3,0,a : rx = a : : ee = "" : continue do
        b = left(a,n - 1) : c = mid(a,n + 1) 'extract key & value
        if ee = "" then
            select case b
                'timeline
                case "start" : write #3,34,35 : d = str(val(c)-1) : rsf = d 'remember start frame
                case "end" : write #3,35,d & "," & str(val(c)-1) 'mapped as frame=x,y
                case "frame" : write #3,34,35,35,c 'if this key exists, it contains chain > csv values
                    rsf = c 'remember start frame
                case "layer" : write #3,34,str(val(c)-1)
                'object/effect/filter
                case "_name" : FindParameterBlock(c)
                    if e = c then
                        ee = e : jj = j : kk = k : write #3,j,k,j,c 'specify array subscript boundaries & object key=value
                    else
                        print #2,a 'log unmapped _name=object
                        c = "Monaural" : FindParameterBlock(c)
                        write #3,j,k,j,c 'dummy filter to replace audio delay
                    end if
                'scene header
                case "name" : write #3,8,c : if c <> "" then write #10,c,h 'scene name
                case "width" : write #3,1,31,9,c 'array subscript boundaries define the beginning & end of a parameter block
                case "height" : write #3,10,c
                case "rate" : write #3,11,c,3,pm & h 'save the file path to the project header
                case "scale" : write #3,12,c
                case "audio_rate" : write #3,13,c
            end select
            continue do
        end if
        tv = ee & ":" & b : FindExoKey(tv) ':?w;tab(10);tv;tab(48);c:sleep
        select case tv 'map as many parameters as possible
            case "Scene change:name"
                'Some transitions have been categorized differently. This requires a redirect to the appropriate parameter block
                if c = "Reel spin" or c = "Shatter" then FindParameterBlock(c) : write #4,0,rx,j,k,j,c : exit select
                write #3,w,c 'name of transition filter (no name defaults to Crossfade)
            case "Scene change:check0" : write #3,jj + 14,c 'Sidespin flag for Reel Rotation
            case "Video file:Playback position" : pp = c 'remember playback position
            case "Video file:file"
                write #3,w,c 'path
                'obtain the video framerate required by AviUtl ExEdit2
                if pp = "1" then d = "0" else fr = GetNativeMp4Framerate(c) : d = str((val(pp)-0.5)/fr) ':?fr,c
                d = format(val(d),"0.000") 'frames to millisecond accuracy
                write #3,w - 2,d ':?w-2,d
            case "Scene:" : write #3,w,c : write #5,c 'remember scene ID
            case "Scene:Playback position"
                d = str((val(c)-0.5)/29.970) 'project frame rate *need to remember it
                d = format(val(d),"0.000") : write #3,w,d
                write #5,val(rsf),d, 'remember start frame & playback position
            case "Scene (audio):Sync with the scene" : close #5
                open "scene linker.lst" for input as #5
                do until eof(5) 'find the correct start frame
                    input #5,m,d,r : if m = val(rsf) then exit do
                loop
                close #5
                open "scene linker.lst" for append as #5
                if c = "1" then FindExoKey("Scene (audio):Playback position") : write #3,w,d
                FindExoKey("Scene (audio):") : write #3,w,r 'scene ID
            case "Audio waveform display:type" : write #3,w,str(val(c) - 1) 'exo type 1-5 vs presets *nfv*
            case "Noise:type" : write #3,w,"Type" & str(val(c) + 1)
            case "Noise:mode" : FindParameterBlock("Pixel synthesis") : GetJPNText
            case "Graphic:type" : FindParameterBlock("Shape type") : GetJPNText
            case "Mask:type" 'there are 19 options (0-18) aup2 has 10 options
                if val(c) > 7 then c = "2" 'fallback to Square mask
                FindParameterBlock("Shape type") : GetJPNText
            case "Gradient:type" : FindParameterBlock("Gradient shape") : GetJPNText
            case "Mirror:type" : FindParameterBlock("Mirror direction") : GetJPNText
            case "Standard drawing:blend" 'there are 13 synthesis modes (0-12) default is Normal
                FindParameterBlock("Blend mode") : GetJPNText
            case "Counter:track3" : FindParameterBlock("Display format") : GetJPNText                
            case "Counter:deco" : FindParameterBlock("Decoration type") : GetJPNText
            case "Text:text" 'convert utf-16 LE hex pairs to ascii codes/unicode points
                'The following equation handles the Basic Multilingual Plane (BMP), which covers characters from chr(0) to chr(65535)
                'Decimal Code = (High Byte * 256) + Low Byte
                d = ""
                for g = 1 to len(c) step 4
                    m = cint("&h" & mid(c,g,2)) : n = cint("&h" & mid(c,g + 2,2)) : d = d & chr((n * 256) + m)
                    if m = 0 and n = 0 then exit for 'upon finding two null characters, ignore rest of encoding
                next
                d = AfxStrReplace(d,chr(10),"") 'remove newline/line feed control character
                d = AfxStrReplace(d,chr(13),"\n") 'replace carriage return with newline escape sequence
                d = AfxStrReplace(d,chr(34),chr(39)) 'replace double quotes with single quotes
                write #3,w,d 'aup2 text property
            case "Text:type" : FindParameterBlock("Decoration type") : GetJPNText
            case "Text:align" 'there are 18 alignment options (integer 0-17) default is Left Align [Top]
                FindParameterBlock("Text alignment") : GetJPNText
            case "Color shift:type" : FindParameterBlock("CS type") : GetJPNText
            case "De-interlacing:type" : FindParameterBlock("Interlace type") : GetJPNText
            case "Glow:type" : FindParameterBlock("Glow shape") : GetJPNText
            case "Luminance Key:type" : FindParameterBlock("Luminance Mode") : GetJPNText
            case "Chroma Key:color_yc" : write #3,w,c 'YCbCr > RGB conversion ?
            case "Volume level:Level" : write #3,w,str(val(c) * 0.390625 + 100) 'scale approximation
            case else
                if e <> tv then continue do
                'if e <> tv then print #2,tv : continue do 'log unmapped keys
                'mapping of single values and csv (denoting change over time)
                if instr(c,",") = 0 or instr(tv,":file") > 0 or instr(tv,":name") > 0 then 'write a single value
                    write #3,w,c : continue do
                end if
                m = instrrev(c,",") + 1 'locate end of csv
                d = mid(c,m) : n = val(d) : FindParameterBlock("Movement type") 'sets integer variable j to start of block
                'INDEX ROUTING
                if n = 1 then
                    tm = f(j,1) 'Rectilinear to Linear Movement,0
                elseif n > 100 and n < 104 then
                    n = n mod 100 : tm = f(j + n,1) 'Rectilinear to Linear Movement,(1-3)
                ElseIf n = 2 Then
                    tm = f(j + 4, 1) 'Interpolated Movement,0 (replaces exo Curve Movement)
                ElseIf n = 3 Then
                    tm = f(j + 5, 1) 'Instant Jump Cut,0
                ElseIf n = 6 Then
                    tm = f(j + 6, 1) 'Random Movement,0
                ElseIf n = 8 Then
                    tm = f(j + 7, 1) 'Repeat / Bounce,0
                ElseIf d = "15@" Then '
                else
                    print " Unmapped movement type: ";c;" @frame ";rsf
                    print #2,"Unmapped movement type: ";c;" @frame ";rsf
                    tm = f(j + 3,1) 'substitute Linear Movement,3
                end if
                tm = CWSTR(tm,CP_UTF8) : write #3,w,left(c,m - 1) & tm
        end select
    loop
    close #1,#2,#3,#4,#5
    print " Parameter mapping concluded."


    'a text file with a BOM has a minimum size of 3 bytes
    if AfxFileLen("redirect.lst") > 3 then
        print " Applying redirects:"
        AfxFileCopy ("exo2aup2.lst","exo2redirect.lst")
        open "redirect.lst" for input as #1
        open "exo2redirect.lst" for input as #2
        open "exo2aup2.lst" for output as #3
        do until eof(1)
            input #1,n,a,j,k,w,c
            do until eof(2)
                line input #2,b : print #3,b : if instr(b,a) = 0 then continue do
                line input #2,b : write #3,j,k,w,c : print tab(7);mid(b,instr(b,chr(34)));" > ";c
                if eof(1) = false then exit do
            loop
        loop
        close #1,#2,#3
    end if


    open "exo2aup2.lst" for input as #1
    open "exo2aup2.dat" for output as #3
    'd = f(w) : d = mid(d,instr(d,".") - 1) : c = format(val(c),d)
    do until eof(1) = true
        input #1,n,a
        if n = 0 then input #1,j,k : write #3,0,a,j,k : continue do
        if n > ubound(f) or n < lbound(f) then ?n,"subscript about to exceed array bounds":sleep 'debugging line
        b = f(n,1) 'acquire aup2 key=default value and save it as a key=exo value
        if instr(b,"effect") = 0 then write #3,n,left(b,instr(b,"=")) & a else write #3,n,b
    loop
    close #1,#3
    
    
    print " Text translation complete."
    open "exo2aup2.dat" for input as #1
    open h for output as #3
    input #1,n,a,j,k 'initialize variables
    do
        do until eof(1) = true
            input #1,n,a : if n = 0 then exit do
            f(n,1) = a 'update the aup2 array with the new key=value
        loop
        for n = j to k 'write the aup2 parameter block
            print #3,f(n,1)
        next
        if eof(1) = true then exit do
        print #3,a 'write the block index
        input #1,j,k 'get array subscript boundaries for the next param block
    loop
    close #1,#3
    print tab(7);fc;". Output: ";h
end sub



sub HeaderAdjustment
    for j = 1 to 7 'skip project header, change scene ID
        line input #2,e : if j > 5 then print #3,AfxStrReplace(e,"0",str(n))
    next j    
end sub



sub FileScan (fspec as string)
    Dim p As CFindFile
    If p.FindFile(fspec) = S_OK Then
        Do
            If p.IsFile then
                if p.FileExt = "exo" Then
                    ParseExo(p.FileNameX)
                elseIf p.FileExt <> "aup2" Then
                    ExtractAupScenes(p.FileNameX)
                end if
            end if
            If p.FindNext = 0 Then Exit Do
        Loop
    End If
    p.Close
end sub



'chdir pm
pm  = curdir
print " Working folder: ";pm
h = "aup2.map" : n = AfxFileScanA(h) + 1 : redim f(1 to n,1 to 2)
print " Mapping array:";ubound(f);" elements"
open h for input as #1
open "parameter.ini" for output as #2
open "exo key.lst" for output as #3
print " Update parameter block index."
print " Update key index." : n = 0
do until eof(1) 'create a subscript table for the mapping array
    line input #1,a : g = g + 1
    if left(a,1) = ":" then b = mid(a,2) : j = g + 1 : vv = true : continue do
    m = instr(a,":") : if m > 1 and g > 38 then f(g,2) = b & mid(a,m) : print #3,f(g,2) : n = n + 1 : continue do
    if a = "" and vv = true then k = g - 1 : write #2,b,j,k : vv = false
loop
close
print " Parameter block count: ";AfxFileScanA("parameter.ini")
print " EXO key count:";n
kill "exo2aup2.log"
open "parameter.ini" for input as #7

open "project.lst" for output as #10
print " Processing extended edit export files:"
FileScan("*.exo") : close : vv = false
print " Examining AviUtl project files:"
FileScan("*.aup")
if vv = false then goto x655
open "project.lst" for input as #1
h = AfxGetFileName(d) & ".aup2" : open h for output as #3
fc = fc + 1 : print fc;". ";d;" >> ";h
print " Matching scenes with their aup2 equivalents . . . combining them in the proper order:"
for n = 0 to 49
    a = scene_array(n) : seek #1,1
    if a = "" then print n : continue for
    do until eof(1)
        input #1,b,c : if a <> b then continue do
        print n;tab(7);b;tab(55);c
        open c for input as #2
        if n = 0 then 'copy project header, root scene header
            line input #2,e : print #3,e : line input #2,e : print #3,e
            line input #2,e : print #3,"file=";pm;h
            do until eof(2) 'copy the remainder of the file
                line input #2,e : print #3,e
                if instr(e,"=") = 0 and instr(e,".") = 0 then rx = e 'remember last index block ID
            loop
            g = val(mid(rx,2))
        else 'append subsequent scenes
            HeaderAdjustment : r = "scene=" & str(n)
            do until eof(2) 'copy file, make index blocks sequential, add scene=n parameter
                line input #2,e
                if instr(e,"=") > 0 then
                    print #3,e : if left(e,6) = "layer=" then print #3,r
                    continue do
                end if
                if instr(e,".") = 0 then g = g + 1 : rx = str(g) else rx = str(g) & mid(e,instr(e,"."),2)
                print #3,"[";rx;"]" 'sequential index block
            loop
        end if
        close #2
        exit do
    loop
    if a <> b then 'append a blank scene
        print n;tab(7);a;tab(51);"[!] File containing the scene name was not found." : open "blank.aup2" for input as #2
        HeaderAdjustment
        do until eof(2)
            line input #2,e : print #3,e
        loop
        close #2
    end if
next n
print tab(7);fc;". Output: ";h
'ParseExo ("test.exo")
'ParseExo ("retro.exo")
'ParseExo ("narrative.exo")
x655:
close : print " File migration is done. Press any key to exit."
sleep : end

