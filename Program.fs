
//Создайте собственные функции для выполнения основных операций над списками (добавление/ 
//удаление/поиск элемента, сцепка двух списков, получение элемента по номеру).

//ПРОВЕРКА ВВОДА ЭЛЕМЕНТОВ В СПИСОК
let Errors (x:string) correct =
    let Rep_x = x.Replace(".", ",")
    match System.Double.TryParse(Rep_x) with
    | (true, value) -> value :: correct  //добавляем число в correct []
    | _ -> 
        printfn "Ошибка! '%s' не является допустимым числом. Символ был пропущен." x
        correct

//СОЕДИНЕНИЕ ДВУХ СПИСКОВ
let rec ConnecLists list1 list2 =
    match list1 with
    | [] -> list2         //если первый список пустой, то вернуть второй список
    | h::t -> 
        h :: ConnecLists t list2 


let rec Operations listElem = 
    DisplayList listElem
    printfn "Выберите функцию:"
    printfn "1) Добавление числа в список"
    printfn "2) Удаление числа из списка"
    printfn "3) Поиск числа в списке"
    printfn "4) Получение элемента по номеру в списке"
    printfn "5) Сцепление со вторым списком"
    printfn "6) Выход"
    
    let choice = System.Console.ReadLine()

    match choice with
    | "1" -> 
        let newNumbers = AddElem listElem
        Operations newNumbers
    | "2" -> 
        let newNumbers = DeleElem listElem
        Operations newNumbers
    | "3" -> 
        SearchElem listElem 
        Operations listElem
    | "4" -> 
        IndexElem listElem 
        Operations listElem
    | "5" -> 
        SecondLists listElem 
    | "6" -> 
        printfn "Выход из программы."
        printfn ""
    | _ -> 
        printfn "Ошибка! Входные данные не являются корректным вариантом."
        Operations listElem


//ДОБАВЛЕНИЕ ЭЛЕМЕНТА В СПИСОК
and AddElem listElem = 
    printfn "" 
    printfn "Введите число для добавления:"
    let input = System.Console.ReadLine()

    let Rep_x = input.Replace(".", ",")
    match System.Double.TryParse(Rep_x) with
    | (true, value) -> 
        value :: listElem      //добавление элемента в начало списка
    | _ -> 
        printfn "Некорректное число. Элемент не добавлен."
        listElem //возврат неизмененного списка


//УДАЛЕНИЕ ЭЛЕМЕНТА В СПИСКЕ
and DeleElem listElem = 
    printfn "" 
    printfn "Введите число для удаления:"
    let input = System.Console.ReadLine()

    let Rep_x = input.Replace(".", ",")
    match System.Double.TryParse(Rep_x) with
    | (true, value) -> 
        let rec Element elem lst =
            match lst with
            | [] -> [] 
            | h::t -> 
                if h = elem then                  //если голова = элемент для удаления
                    Element elem t                //не добавляем
                else                              //если голова <> элемент для удаления
                    h :: Element elem t           //добавляем
                
        Element value listElem
    | _ -> 
        printfn "Некорректное число. Элемент не удален."
        listElem 


//ПОИСК ЭЛЕМЕНТА В СПИСКЕ
and SearchElem listElem = 
    printfn "" 
    printfn "Введите число для поиска:"
    let input = System.Console.ReadLine()

    let Rep_x = input.Replace(".", ",")
    match System.Double.TryParse(Rep_x) with
    | (true, value) -> 

        let rec Search elem list index correct =
            match list with
            | [] -> correct                   
            | h::t -> 
                if h = elem then 
                    Search elem t (index + 1) (index :: correct)    //увеличиваем индекс и добавляем текущий индекс к correct
                else 
                     Search elem t (index + 1) correct //поиск без добавления индекса

        //рекурсивная функция начало с индекса 1 и пустого correct
        let indices = Search value listElem 1 [] 

        match indices with
        | [] -> printfn "Элемент %f не найден в списке." value //если список индексов пуст, вывод сообщения о том, что элемент не найден
        | _ -> printfn "Элемент %f найден в списке на индексах: %A" value (List.rev indices) //разворачивание списка для вывода индексов в порядке возрастания

    | _ -> 
        printfn "Некорректное число для поиска."


//НАХОЖДЕНИЕ ЭЛЕМЕНТА В СПИСКЕ ПО ИНДЕКСУ
and IndexElem listElem =
    printfn "" 
    printfn "Введите индекс элемента:"
    let input = System.Console.ReadLine()
    
    match System.Int32.TryParse(input) with
    | (true, index) when index > 0 ->     //индекс неотрицательный
        let newindex = index - 1 

        let rec GetElement idx lst =
            match lst with
            | [] -> None 
            | h::t -> 
                if idx = 0 then
                    Some h                 //возвращение элемента
                else
                    GetElement (idx - 1) t //продолжение поиска

        match GetElement newindex listElem with
        | Some value -> printfn "Элемент на индексе %d: %f" index value
        | None -> printfn "Индекс %d превышает размер списка." index
    | _ -> 
        printfn "Некорректный индекс."


//СОЗДАНИЕ ВТОРОГО СПИСКА
and SecondLists listElem =
    printfn "" 
    printfn "Введите второй список чисел через пробел:"
    let input = System.Console.ReadLine()

    let secondList = 
        input.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.fold (fun correct x -> Errors x correct) []
        |> List.rev 

    let newlistElem = ConnecLists listElem secondList   //соединение двух списков
    printfn "Сцепленный список: %A" newlistElem
    Operations newlistElem 
      
     
//ВЫВОД СПИСКА
and DisplayList listElem =
    printfn "" 
    printfn "Текущий список: %A" listElem


//ВВОД СПИСКА
let rec input3 () =
    printfn "" 
    printfn "Введите числа через пробел и нажмите Enter:"
    let input = System.Console.ReadLine()
    
    //преобразование строки в список чисел
    let listElem =
        input.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)  //разделение строки (убираем лишние пробелы)
        |> Array.fold (fun correct x -> Errors x correct) []   //для каждого элемента вызываем функцию проверки на корректность correct = []
        |> List.rev                                    //переворачиваем список
    
    Operations listElem


//----------------------------------------------------------------------------------------------


//Найти произведение нечётных цифр натурального числа. 
let rec Number2 n =
    if n = 0 then
        (1, false)
    else
        let ost = n % 10 
        let n1 = n / 10 
        if ost % 2 <> 0 then
            let (n2, flag) = Number2 n1
            (ost * n2, true)
        else
            Number2 n1  

//вывод
let Print n result flag =
    if flag then
        printfn "Произведение нечётных цифр числа %d равно %d" n result
    else
        printfn "В числе %d нет нечётных цифр. Произведение = 0" n
    printfn ""

let rec input2 () =
    printf "Введите натуральное число: "
    let input = System.Console.ReadLine()

    if input.Length > 10 || (input.Length = 10 && input > "2147483647") then 
        printfn "Ошибка! Введите корректное натуральное число, не превышающее %d." System.Int32.MaxValue
        input2 ()  
    else
        match System.Int32.TryParse(input) with
        | (true, n) when n > 0 -> 
            let (result, flag) = Number2 n
            Print n result flag 
        | _ -> 
            printfn "Ошибка! Введите корректное натуральное число."
            input2 ()


//Сформировать список из чисел, на 1 больших, чем вводимые значения. 
let Number1 (x: string) =
    let Rep_x = x.Replace(".", ",")
    match System.Double.TryParse(Rep_x) with
    | (true, num) -> 
        Some(num + 1.0)
    | _ -> 
        printfn "Ошибка! '%s' не является допустимым числом. Символ был пропущен. " x
        None 

let rec input1 () =
    printfn "Введите числа через пробел и нажмите Enter:"
    let input = System.Console.ReadLine()
    
    //преобразование строки в список чисел
    let result =
        input.Split(' ')              //пробел как разделитель (массив строк)
        |> Array.toList               //массив в список - преобразование
        |> List.choose Number1        //применение функции к каждому числу

    if List.isEmpty result then
        input1 ()                    // Рекурсивный вызов для повторного ввода
    else
        printfn "Список чисел, на 1 больше, чем вводимые значения: \n %A" result
        printfn ""


//старт
let rec Start () =
    printfn "Выберите задание: "
    printfn "1 задание"
    printfn "2 задание"
    printfn "3 задание"
    printfn "Для выхода введите 'q' и Enter"

    let number = System.Console.ReadLine()

    match number with
    | "1" -> 
        printfn ""
        printfn "----:: Список из чисел, на 1 больших, чем вводимые значения ::----"
        input1()
        Start() 
    | "2" -> 
        printfn ""
        printfn "----:: Произведение нечётных цифр натурального числа ::----"
        input2()
        Start() 
    | "3" -> 
        printfn "" 
        printfn "----:: Работа со списками ::----" 
        input3()
        Start() 
    | "q" -> 
        printfn "Выход из программы." 
    | _ -> 
        printfn "Ошибка! Входные данные не являются натуральным числом."  
        printfn ""  
        Start() 

Start() 