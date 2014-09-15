namespace FSharpExt

type Matrix<'innerType, 'rows, 'columns> = LengthList<LengthList<'innerType, 'rows>, 'columns>

module Utils =
    /// <summary>
    /// Gets length based on type
    /// </summary>
    /// <param name="list"></param>
    let inline getListLength (_ : 'a list) = !!Unchecked.defaultof<'a>

type ReadMat =
    | ReadMat
    static member inline (=<=) (ReadMat, LengthList(_ : LengthList<_, 'numRows> list, numColsN)) = fun list ->
        let numRows = !!Unchecked.defaultof<'numRows>
        let numCols = !!numColsN
        let columns = List.map (Func.flip LengthList.ofList numRows) list
        LengthList.ofList columns numCols

module Matrix = 
    let inline read list : 'a = (ReadMat =<= Unchecked.defaultof<'a>) list

    let inline ofList (list : ^a list list) rows columns = 
                                                           
        let toLL (list : 'a list) = LengthList.ofList list rows
        LengthList.ofList (List.map toLL list) columns
    
    let getList (LengthList(l, _)) = l
    let getNumRows (LengthList(LengthList(_, rows) :: _, _)) = rows
    let getNumCols (LengthList(_, cols)) = cols
    
    let inline transpose mat = 
        let (LengthList(cols, _)) = mat
        let numRows = getNumRows mat
        let numCols = getNumCols mat
        
        let rec columnToRowLists columns = 
            match List.head columns with
            | [] -> []
            | _ :: _ -> List.map List.head columns :: columnToRowLists (List.map List.tail columns)
        
        let columnLists = List.map getList cols
        let rowLists = columnToRowLists columnLists
        ofList rowLists numCols numRows
        
    /// <summary>
    /// Read the matrix with the inner lists representing rows instead of columns (better for aligning code)
    /// </summary>
    let inline readAligned list = read list |> transpose

    let inline show mat : string =
        let (LengthList(_, colNum)) = mat
        let (LengthList(rows, rowNum)) = transpose mat
        Numerals.showPlain rowNum + "x" + Numerals.showPlain colNum + "\n" + (List.map LengthList.showPlain rows |> String.concat "\n")
    
    let inline mult mat1 mat2 = 
        let mT = transpose mat1
        let multCol column = LengthList.dot column
        ofList [ for f in List.map multCol (getList mat2) -> List.map f (getList mT) ] (getNumRows mat1) 
            (getNumCols mat2)
