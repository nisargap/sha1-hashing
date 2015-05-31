Public Class SHA1
    ' Created by: Nisarga Patel
    ' License: MIT
    'The purpose of this project is to perform the SHA-1 hash computational method as outlined in NIST publication FIPS 180-1 and FIPS 180-2.
    'I forgot how to do a few things so I had to refer back to the Column Permutation program for reference on DataGridView. 
    'The column permutation program was in VB 2008 so a few things were different for datagridviews
    'Before I do anything I need to establish a DataTable which I will be using as the source for my DataGridView
    Public roundval As New DataTable
    Public col As New Integer
    Public row As New Integer
    Function ConvertToBin(ByVal a As String, ByVal b As Integer)
        'I modified my convert to binary code so that it has a "b" parameter for base, so now I can convert from any base 
        'I also modified it so that instead of 4 characters it would always output as 8 character 
        Dim stringA As String = "00000000" & Convert.ToString(Convert.ToInt32(a, b), 2)
        Dim stringB As String = ""
        stringB &= stringA.Substring(stringA.Length - 8, 8) & ""
        Return stringB
    End Function
    Function ConvertToHex(ByVal a As String)
        'I modified my convert to binary code so that it has a "b" parameter for base, so now I can convert from any base 
        'I also modified it so that instead of 4 characters it would always output as 8 character 
        Dim stringA As String = "00000000" & Conversion.Hex(a)
        Dim stringB As String = ""
        stringB &= stringA.Substring(stringA.Length - 8, 8) & ""
        Return stringB
    End Function
    Function ConvertToBin32(ByVal a As String, ByVal b As Integer)
        'I modified my convert to binary code so that it has a "b" parameter for base, so now I can convert from any base 
        'I also modified it so that instead of 4 characters it would always output as 64 character 
        Dim stringA As String = "00000000000000000000000000000000" & Convert.ToString(Convert.ToInt64(a, b), 2)
        Dim stringB As String = ""
        stringB &= stringA.Substring(stringA.Length - 32, 32) & ""
        Return stringB
    End Function
    Function ConvertToBin64(ByVal a As String, ByVal b As Integer)
        'I modified my convert to binary code so that it has a "b" parameter for base, so now I can convert from any base 
        'I also modified it so that instead of 4 characters it would always output as 64 character 
        Dim stringA As String = "0000000000000000000000000000000000000000000000000000000000000000" & Convert.ToString(Convert.ToInt32(a, b), 2)
        Dim stringB As String = ""
        stringB &= stringA.Substring(stringA.Length - 64, 64) & ""
        Return stringB
    End Function
    Function rotl(ByVal x As String, ByVal amountofbits As Integer)
        x = ConvertToBin32(CDec("&H" & ConvertToHex(x)), 10)
        'Binary Input
        Dim stringA As String = ""
        stringA = x.Substring(amountofbits, x.Length - amountofbits) & x.Substring(0, amountofbits)
        Return Convert.ToInt32(stringA, 2)
    End Function
    Function rotlhex(ByVal x As String, ByVal amountofbits As Integer)
        x = ConvertToBin32(x, 16)
        'Binary Input
        Dim stringA As String = ""
        stringA = x.Substring(amountofbits, x.Length - amountofbits) & x.Substring(0, amountofbits)
        Return Convert.ToInt32(stringA, 2)
    End Function
    Private Sub output_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles output.Click
        hexvals.Text = ""
        block.Text = ""
        If row <> 0 Then
            For a = 0 To 79
                roundval.Rows.Clear()
            Next
        End If
        If col <> 0 Then
            For b = 0 To 7
                roundval.Columns.Clear()
            Next
        End If
        'The first thing I need to do is Plaintext Pad -> ASCII -> Hexadecimal -> and seee if Example is the same
        For i = 0 To plaintxt.Text.Length - 1
            hexvals.Text &= Conversion.Hex((Asc(plaintxt.Text.Substring(i, 1)))).ToString
        Next
        Dim binarytext As String = ""
        For i = 0 To plaintxt.Text.Length - 1
            binarytext &= ConvertToBin(Asc(plaintxt.Text(i)), 10)
        Next
        Dim LengthInBin As String = ConvertToBin64(Len(binarytext), 10)
        binarytext = binarytext & "1"
        For i = 0 To (448 - (Len(binarytext) + 1))
            binarytext &= "0"
        Next
        block.Text = binarytext & LengthInBin
        'I need to start off by initializing the number of rows and columns in the DataGridView.
        'From the example it looks like there are 8 columns, not counting i of course, and 80 rows.
        'I dimensionalized variables for each of the 8 columns
        roundval.Clear()
        roundval.Columns.Add("i")
        roundval.Columns.Add("a")
        roundval.Columns.Add("b")
        roundval.Columns.Add("c")
        roundval.Columns.Add("d")
        roundval.Columns.Add("e")
        roundval.Columns.Add("f")
        roundval.Columns.Add("w(i)")
        For i = 0 To 79
            roundval.Rows.Add(i)
        Next
        'Next I need to dimensionalize the initial alphabet values
        'VARIABLES_______________________________________________
        Dim aval As String = "67452301"
        Dim bval As String = "EFCDAB89"
        Dim cval As String = "98BADCFE"
        Dim dval As String = "10325476"
        Dim eval As String = "C3D2E1F0"
        Dim tval As String = ""
        Dim kval As New List(Of String)
        'END OF VARIABLES_______________________________________
        'THE INITAL VALUES OF THE VARIABLES
        'KVALS__________________________________________________
        For q As Integer = 0 To 19
            kval.Add(CDec("&H" & "5A827999"))
        Next
        For q As Integer = 20 To 39
            kval.Add(CDec("&H" & "6ED9EBA1"))
        Next
        For q As Integer = 40 To 59
            kval.Add(CDec("&H" & "8F1BBCDC"))
        Next
        For q As Integer = 60 To 79
            kval.Add(CDec("&H" & "CA62C1D6"))
        Next
        'END OF KVALS____________________________________________
        'WVALS___________________________________________________
        Dim wvals As New List(Of String)
        For i As Integer = 0 To block.Text.Length - 1 Step 32
            'wvals.Add(CDec("&H" & Convert.ToString(Convert.ToInt32((block.Text.Substring(i, 32)), 2), 16)))
            wvals.Add(CDec("&H" & Convert.ToString(Convert.ToInt32(block.Text.Substring(i, 32), 2), 16)))
        Next
        For i = 0 To (block.Text.Length / 32) - 1
            roundval.Rows(i)(7) = Conversion.Hex(wvals(i))
        Next
        For i = 16 To 79
            wvals.Add(rotl((wvals(i - 3) Xor wvals(i - 8) Xor wvals(i - 14) Xor wvals(i - 16)), 1))
            roundval.Rows(i)(7) = Conversion.Hex(wvals(i))
        Next
        'END OF WVALS_____________________________________________
        Dim f As Double
        For i = 0 To 19
            f = ((CDec("&H" & bval) And CDec("&H" & cval)) Or ((Not CDec("&H" & bval) And CDec("&H" & dval))))
            roundval.Rows(i)(6) = Conversion.Hex(f)
            roundval.Rows(i)(0) = i
            tval = Conversion.Hex(((rotlhex(aval, 5) + f + CDec("&H" & eval) + wvals(i) + kval(i)) Mod 2 ^ 32))
            eval = dval
            roundval.Rows(i)(5) = eval
            dval = cval
            roundval.Rows(i)(4) = dval
            cval = Conversion.Hex(rotlhex(bval, 30))
            roundval.Rows(i)(3) = cval
            bval = aval
            roundval.Rows(i)(2) = bval
            aval = tval
            roundval.Rows(i)(1) = aval
        Next
        For i = 20 To 39
            f = (CDec("&H" & bval) Xor CDec("&H" & cval) Xor CDec("&H" & dval))
            roundval.Rows(i)(6) = Conversion.Hex(f)
            roundval.Rows(i)(0) = i
            tval = Conversion.Hex(((rotlhex(aval, 5) + f + CDec("&H" & eval) + wvals(i) + kval(i)) Mod 2 ^ 32))
            eval = dval
            roundval.Rows(i)(5) = eval
            dval = cval
            roundval.Rows(i)(4) = dval
            cval = Conversion.Hex(rotlhex(bval, 30))
            roundval.Rows(i)(3) = cval
            bval = aval
            roundval.Rows(i)(2) = bval
            aval = tval
            roundval.Rows(i)(1) = aval
        Next
        For i = 40 To 59
            f = (CDec("&H" & bval) And CDec("&H" & cval)) Xor (CDec("&H" & bval) And CDec("&H" & dval)) Xor (CDec("&H" & cval) And CDec("&H" & dval))
            roundval.Rows(i)(6) = Conversion.Hex(f)
            roundval.Rows(i)(0) = i
            tval = Conversion.Hex(((rotlhex(aval, 5) + f + CDec("&H" & eval) + wvals(i) + kval(i)) Mod 2 ^ 32))
            eval = dval
            roundval.Rows(i)(5) = eval
            dval = cval
            roundval.Rows(i)(4) = dval
            cval = Conversion.Hex(rotlhex(bval, 30))
            roundval.Rows(i)(3) = cval
            bval = aval
            roundval.Rows(i)(2) = bval
            aval = tval
            roundval.Rows(i)(1) = aval
        Next
        For i = 60 To 79
            f = ((CDec("&H" & bval) Xor CDec("&H" & cval) Xor CDec("&H" & dval)))
            roundval.Rows(i)(6) = Conversion.Hex(f)
            roundval.Rows(i)(0) = i
            tval = Conversion.Hex(((rotlhex(aval, 5) + f + CDec("&H" & eval) + wvals(i) + kval(i)) Mod 2 ^ 32))
            eval = dval
            roundval.Rows(i)(5) = eval
            dval = cval
            roundval.Rows(i)(4) = dval
            cval = Conversion.Hex(rotlhex(bval, 30))
            roundval.Rows(i)(3) = cval
            bval = aval
            roundval.Rows(i)(2) = bval
            aval = tval
            roundval.Rows(i)(1) = aval
        Next
        roundtable.DataSource = roundval
        Dim avalOld As String = "67452301"
        Dim bvalOld As String = "EFCDAB89"
        Dim cvalOld As String = "98BADCFE"
        Dim dvalOld As String = "10325476"
        Dim evalOld As String = "C3D2E1F0"
        hashed.Text = Conversion.Hex(((CDec("&H" & avalOld) + CDec("&H" & aval)) Mod 2 ^ 32)) & Conversion.Hex(((CDec("&H" & bvalOld) + CDec("&H" & bval)) Mod 2 ^ 32)) & Conversion.Hex(((CDec("&H" & cvalOld) + CDec("&H" & cval)) Mod 2 ^ 32)) & Conversion.Hex(((CDec("&H" & dvalOld) + CDec("&H" & dval)) Mod 2 ^ 32)) & Conversion.Hex(((CDec("&H" & evalOld) + CDec("&H" & eval)) Mod 2 ^ 32))
        For i = 0 To 7
            roundtable.Columns(i).SortMode = DataGridViewColumnSortMode.NotSortable
        Next
        row = 79
        col = 7
    End Sub

    Private Sub example_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles example.Click
        plaintxt.Text = "This is an example of SHA-1."
    End Sub
End Class
