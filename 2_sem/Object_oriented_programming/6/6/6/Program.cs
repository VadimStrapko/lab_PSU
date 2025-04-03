public class General<T> 
{
    public T item;
    public General<T>[] intArray;
    private int currentIndex;

    public void Sort()
    {
        if (currentIndex == 0)
            throw new Exception("Массив пустой.");
        
        Array.Sort(intArray, 0, currentIndex, Comparer<General<T>>.Create((x, y) => Comparer<T>.Default.Compare(x.item, y.item)));
    }

    public int FindItem(T obj)
    {
        for (int i = 0; i < currentIndex; i++)
        {
            if (EqualityComparer<T>.Default.Equals(intArray[i].item, obj))
                return i; 
        }

        return -1; 
    }

    public void ShowInfoAdmin()
    {
        for (int i = 0; i < currentIndex; i++)
        {
            if (intArray[i].item is Admin admin) 
            {
                admin.ShowInfo(); 
            }
            else
            {
                Console.WriteLine(intArray[i].item); 
            }
        }
    }

    public void ShowInfoPrem()
    {
        for (int i = 0; i < currentIndex; i++)
        {
            if (intArray[i].item is Premium premium) 
            {
                premium.ShowInfo(); 
            }
            else
            {
                Console.WriteLine(intArray[i].item); 
            }
        }
    }

    public T Min()
    {
        if (currentIndex == 0)
            throw new Exception("Массив пустой.");

        T minVal = intArray[currentIndex - 1].item;
        for(int i = 1; i < currentIndex; i++)
        {
            if(Comparer<T>.Default.Compare(intArray[i].item, minVal) < 0)
                minVal = intArray[i].item;
        }

        return minVal;
    }

    public T Max()
    {
        if(currentIndex == 0)
            throw new Exception("Массив пустой.");

        T maxVal = intArray[currentIndex - 1].item;
        for (int i = 1;i < currentIndex; i++)
        {
            if (Comparer<T>.Default.Compare(intArray[i].item, maxVal) > 0)
                maxVal = intArray[i].item;
        }

        return maxVal;
    }
        

    public General(int size)
    {
       if (size <= 0)
            throw new Exception("Неккоректный ввод(число должно быть положительным).");
       
        intArray = new General<T>[size];
    }

    public void AddElement(T value)
    {
        if (currentIndex >= intArray.Length)
            throw new Exception("Массив переполнен.");

        intArray[currentIndex] = new General<T>(1);
        intArray[currentIndex].item = value;
        currentIndex++;
    }

    public General<T> GetElement(int index)
    {
        if (currentIndex < 0 || index > currentIndex) 
            throw new Exception("Неккоректный ввод.");

        return intArray[index];
    }

    public void ShowInfo()
    {
        for (int i = 0; i < currentIndex; i++)
            Console.WriteLine(intArray[i].item);
    }
}

abstract class User
{
    private string login;
    private string password;
    private string role;

    public User(string login, string password, string role)
    {
        Console.WriteLine("Конструктор класса User с параметрами вызван");
        this.login = login;
        this.password = password;
        this.role = role;
    }

    public User()
    {
        Console.WriteLine("Конструктор класса User без параметров вызван");
    }


    public User(User user)
    {
        Console.WriteLine("Конструктор класса User копирования вызван");
        this.login = user.login;
        this.password = user.password;
        this.role = user.role;
    }

    public void MoveFrom(Premium user)
    {
        Console.WriteLine("Метод перемещения класса User вызван");
        this.login = user.login;
        this.password = user.password;
        this.role = user.role;

        user.login = null;
        user.password = null;
        user.role = null;
    }

    ~User()
    {
        Console.WriteLine("Деструктор класса User вызван");
    }

    public virtual void ShowInfo()
    {
        Console.WriteLine($"Логин: {login} Пароль: {password} Роль: {role}");
    }

    public string GetLogin() => login;
    public string GetRole() => role;
    public string GetPassword() => password;

    public void ChangePassword(string newPassword)
    {
        password = newPassword;
    }

    public void ChangeRole(string newRole)
    {
        role = newRole;
    }

}

class CustomNumber
{

    public int Value;
    public CustomNumber(int value)
    {
        Value = value;
    }

    public static CustomNumber operator <<(CustomNumber number, int shift)
    {
        return new CustomNumber(number.Value << shift);
    }

    public string ToString()
    {
        return Value.ToString();
    }
}

class Admin : User, IComparable<Admin>
{
    public string departament;

    public Admin(string login, string password, string role, string departament) : base(login, password, role)
    {
        this.departament = departament;
        Console.WriteLine("Конструктор класса Admin с параметрами вызван");
    }

    public int CompareTo(Admin other)
    {
        if (other == null) return 1;
        return this.departament.CompareTo(other.departament);
    }

    public static bool operator <(Admin a, Admin b)
    {
        return a.departament.CompareTo(b.departament) < 0; 
    }

    public static bool operator >(Admin a, Admin b)
    {
        return a.departament.CompareTo(b.departament) > 0;
    }

    public void ChangeDepartament(string newDepartament)
    {
        departament = newDepartament;
    }

    public Admin() : base()
    {
        Console.WriteLine("Конструктор класса Admin без параметров вызван");
    }

    public Admin(Admin other) : base(other)
    {
        this.departament = other.departament;
        Console.WriteLine("Конструктор копирования класса Admin вызван");
    }

    ~Admin()
    {
        Console.WriteLine("Деструктор класса Admin вызван");
    }

    public static bool operator ==(Admin a, Admin b)
    {
        if (ReferenceEquals(a, null))
        {
            return ReferenceEquals(b, null);
        }
        if (ReferenceEquals(b, null))
        {
            return false;
        }

        return a.GetLogin() == b.GetLogin();
    }

    public static bool operator !=(Admin a, Admin b)
    {
        return !(a == b);
    }

    public void Manager()
    {
        Console.WriteLine("Пользователь с таким логином (" + GetLogin() + ") управляет таким департаментом " + departament);
    }

    public override void ShowInfo()
    {
        base.ShowInfo();
        Console.WriteLine("Отдел: " + departament);
    }
}

class Premium : User, IComparable<Premium> 
{
    public DateTime subDate;

    public Premium(string login, string password, string role, DateTime subDate) : base(login, password, role)
    {
        this.subDate = subDate;
        Console.WriteLine("Конструктор класса Premium с параметрами вызван");
    }

    public Premium() : base()
    {
        Console.WriteLine("Конструктор класса Premium без параметров вызван");
    }

    public Premium(Premium other) : base(other)
    {
        this.subDate = other.subDate;
        Console.WriteLine("Конструктор копирования класса Premium вызван");
    }
    public int CompareTo(Premium other)
    {
        if (other == null) return 1;
        return this.subDate.CompareTo(other.subDate);
    }
    ~Premium()
    {
        Console.WriteLine("Деструктор класса Premium вызван");
    }

    public void Test()
    {
        if (DateTime.Now <= subDate)
            Console.WriteLine("Подписка не истекла");
        else Console.WriteLine("Подписка истекла");

    }

    public override void ShowInfo()
    {
        base.ShowInfo();
        Console.WriteLine("Подписка действует до: " + subDate);
    }

    public static bool operator <(Premium a, Premium b)
    {
        return a.subDate < b.subDate;
    }

    public static bool operator >(Premium a, Premium b)
    {
        return a.subDate > b.subDate;
    }
}

class Application
{
    private string name;
    private int volume;
    private List<User> users;

    public Application(string name, int volume)
    {
        this.name = name;
        this.volume = volume;
        users = new List<User>();
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    public static Application operator +(Application app, User user)
    {
        app.Add(user);
        return app;
    }

    public static Application operator ++(Application app)
    {
        User defAdmin = new Admin("def_login_lab_5", "def_password", "def_role", "def_departament");
        User defPremium = new Premium("def_login_lab_5", "def_password", "def_role", DateTime.Now);
        app.Add(defAdmin);
        app.Add(defPremium);
        return app;
    }

    public User this[int index]
    {
        get
        {
            if (index >= 0 && index < users.Count)
                return users[index];
            else
                throw new Exception("Некорректный ввод");
        }
        set
        {
            if (index >= 0 && index < users.Count)
                users[index] = value;
            else
                throw new Exception("Некорректный ввод");
        }

    }

    public static Application operator <<(Application app, string message)
    {
        Console.WriteLine(message);
        app.ShowUser();
        return app;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public void Add(User user)
    {
        users.Add(user);
    }


    public void Remove(string login)
    {
        users.RemoveAll(u => u.GetLogin() == login);
    }

    public void RedactUser(string login, string newPassword, string newRole)
    {
        foreach (var user in users)
        {
            if (user.GetLogin() == login)
            {
                user.ChangePassword(newPassword);
                user.ChangeRole(newRole);
                return;
            }
        }
    }

    public virtual void CreateUser(string login, string password, string role)
    {
        Premium user = new Premium(login, password, role, DateTime.Now);
        Add(user);
    }

    public void CreateCopies(Premium user, int count)
    {
        for (int i = 0; i < count; i++)
        {
            Premium copy = new Premium(user);
            Add(copy);
        }
    }



    public void ShowUser()
    {

        foreach (var user in users)
        {
            user.ShowInfo();

            if (user is Admin admin)
            {
                Console.WriteLine($"Отдел: {admin.departament}");
            }

            if (user is Premium premium)
            {
                Console.WriteLine($"Подписка действует до: {premium.subDate.ToShortDateString()}");
            }
        }
    }

    class Program
    {
        static void Main()
        {
            Console.WriteLine("//...........................................6lab...............................................................//");
            General<int> intArray = new General<int>(5);
            intArray.AddElement(10);
            intArray.AddElement(3);
            intArray.AddElement(20);
            intArray.AddElement(0);
            intArray.AddElement(111);

            Console.WriteLine("Массив типа int: ");
            intArray.ShowInfo();

            Console.WriteLine("Мнимальный элемент: " + intArray.Min());
            Console.WriteLine("Максимальный элемент: " + intArray.Max());

            intArray.Sort();
            Console.WriteLine("\nОтсортированный массив: ");
            intArray.ShowInfo();

            Console.WriteLine("\nFindItem int");
            int indexOne = intArray.FindItem(10);
            if (indexOne != -1)
                Console.WriteLine("Элемент найден по индексу: " + indexOne);
            else Console.WriteLine("Элемент по индексу не найден");

            int indexTwo = intArray.FindItem(111);
            if (indexTwo != -1)
                Console.WriteLine("Элемент найден по индексу: " + indexTwo);
            else Console.WriteLine("Элемент по индексу не найден");

            General<char> charArray = new General<char>(5);
            charArray.AddElement('b');
            charArray.AddElement('a');
            charArray.AddElement('d');
            charArray.AddElement('c');
            charArray.AddElement('e');

            Console.WriteLine("\nМассив типа char: ");
            charArray.ShowInfo();

            charArray.Sort();
            Console.WriteLine("\nОтсортированный массив: ");
            charArray.ShowInfo();

            Console.WriteLine("Мнимальный элемент: " + charArray.Min());
            Console.WriteLine("Максимальный элемент: " + charArray.Max());

            Console.WriteLine("\nFindItem char");
            int charOne = charArray.FindItem('a');
            if (charOne != -1)
                Console.WriteLine("Элемент найден по индексу: " + charOne);
            else Console.WriteLine("Элемент по индексу не найден");

            int charTwo = charArray.FindItem('1');
            if (charTwo != -1)
                Console.WriteLine("Элемент найден по индексу: " + charTwo);
            else Console.WriteLine("Элемент по индексу не найден");

            General<Admin> adminArray = new General<Admin>(5);
            adminArray.AddElement(new Admin("lab6", "pass1", "admin1", "IT"));
            adminArray.AddElement(new Admin("lab6", "pass2", "admin2", "ВСИС"));
            adminArray.AddElement(new Admin("lab6", "pass3", "admin3", "КСИС"));
            adminArray.AddElement(new Admin("lab6", "pass4", "admin4", "АИСД"));
            adminArray.AddElement(new Admin("lab6", "pass5", "admin5", "ВТ"));

            Console.WriteLine("\nМассив Admin: ");
            adminArray.ShowInfoAdmin();

            adminArray.Sort();
            Console.WriteLine("\nОтсортированный массив: ");
            adminArray.ShowInfoAdmin();

            General<Premium> premiumArray = new General<Premium>(5);
            premiumArray.AddElement(new Premium("lab6", "pass1", "prem1", new DateTime(2021, 1, 1)));
            premiumArray.AddElement(new Premium("lab6", "pass2", "prem2", new DateTime(2022, 2, 2)));
            premiumArray.AddElement(new Premium("lab6", "pass3", "prem3", new DateTime(2023, 3, 3)));
            premiumArray.AddElement(new Premium("lab6", "pass4", "prem4", new DateTime(2024, 4, 4)));
            premiumArray.AddElement(new Premium("lab6", "pass5", "prem5", new DateTime(2025, 5, 5)));


            Console.WriteLine("\nМассив Premium: ");
            premiumArray.ShowInfoPrem();

            premiumArray.Sort();
            Console.WriteLine("\nОтсортированный массив: ");
            premiumArray.ShowInfoPrem();

            
            Console.WriteLine("//...........................................6lab...............................................................//");
            string choice = Console.ReadLine();
            switch (choice)
            {
                case "1":
                    Console.Write("Введите размер массива int: ");
                    int size = int.Parse(Console.ReadLine());
                    General<int> intArray1 = new General<int>(size);

                    for (int i = 0; i < size; i++)
                    {
                        Console.Write($"Введите {i + 1}-й элемент массива: ");
                        int value = int.Parse(Console.ReadLine());
                        intArray1.AddElement(value);
                    }

                    Console.WriteLine("\nМассив типа int: ");
                    intArray1.ShowInfo();
                    Console.WriteLine("Минимальный элемент: " + intArray1.Min());
                    Console.WriteLine("Максимальный элемент: " + intArray1.Max());

                    intArray1.Sort();
                    Console.WriteLine("\nОтсортированный массив: ");
                    intArray1.ShowInfo();

                    Console.WriteLine("\nПоиск элемента в массиве int");
                    Console.Write("Введите элемент для поиска: ");
                    int searchValue = int.Parse(Console.ReadLine());
                    int index = intArray1.FindItem(searchValue);
                    if (index != -1)
                        Console.WriteLine("Элемент найден по индексу: " + index);
                    else
                        Console.WriteLine("Элемент не найден.");
                    break;
                case "2":
                    Console.Write("Введите размер массива char: ");
                    int size1 = int.Parse(Console.ReadLine());
                    General<char> charArray1 = new General<char>(size1);

                    for (int i = 0; i < size1; i++)
                    {
                        Console.Write($"Введите {i + 1}-й элемент массива: ");
                        char value = Console.ReadKey().KeyChar;
                        Console.WriteLine();
                        charArray1.AddElement(value);
                    }

                    Console.WriteLine("\nМассив типа char: ");
                    charArray1.ShowInfo();

                    charArray1.Sort();
                    Console.WriteLine("\nОтсортированный массив: ");
                    charArray1.ShowInfo();

                    Console.WriteLine("Минимальный элемент: " + charArray1.Min());
                    Console.WriteLine("Максимальный элемент: " + charArray1.Max());

                    Console.WriteLine("\nПоиск элемента в массиве char");
                    Console.Write("Введите элемент для поиска: ");
                    char searchValue1 = Console.ReadKey().KeyChar;
                    Console.WriteLine(); 
                    int charIndex = charArray1.FindItem(searchValue1);
                    if (charIndex != -1)
                        Console.WriteLine("Элемент найден по индексу: " + charIndex);
                    else
                        Console.WriteLine("Элемент не найден.");
                    break;
                case "3":
                    Console.Write("Введите размер массива Admin: ");
                    int size3 = int.Parse(Console.ReadLine());
                    General<Admin> adminArray3 = new General<Admin>(size3);

                    for (int i = 0; i < size3; i++)
                    {
                        Console.WriteLine($"Введите данные для {i + 1}-го админа:");
                        Console.Write("Логин: ");
                        string login = Console.ReadLine();
                        Console.Write("Пароль: ");
                        string password = Console.ReadLine();
                        Console.Write("Роль: ");
                        string role = Console.ReadLine();
                        Console.Write("Департамент: ");
                        string department = Console.ReadLine();

                        adminArray3.AddElement(new Admin(login, password, role, department));
                    }

                    Console.WriteLine("\nМассив Admin: ");
                    adminArray.ShowInfoAdmin();

                    adminArray.Sort();
                    Console.WriteLine("\nОтсортированный массив: ");
                    adminArray.ShowInfoAdmin();
                    break;
                case "4":
                    Console.Write("Введите размер массива Premium: ");
                    int size4 = int.Parse(Console.ReadLine());
                    General<Premium> premiumArray4 = new General<Premium>(size4);

                    for (int i = 0; i < size4; i++)
                    {
                        Console.WriteLine($"Введите данные для {i + 1}-го премиум-пользователя:");
                        Console.Write("Логин: ");
                        string login = Console.ReadLine();
                        Console.Write("Пароль: ");
                        string password = Console.ReadLine();
                        Console.Write("Роль: ");
                        string role = Console.ReadLine();
                        Console.Write("Дата подписки (yyyy-mm-dd): ");
                        DateTime subDate = DateTime.Parse(Console.ReadLine());

                        premiumArray4.AddElement(new Premium(login, password, role, subDate));
                    }

                    Console.WriteLine("\nМассив Premium: ");
                    premiumArray4.ShowInfoPrem();

                    premiumArray4.Sort();
                    Console.WriteLine("\nОтсортированный массив: ");
                    premiumArray4.ShowInfoPrem();
                    break;
                case "5":
                    return;
                default:
                    Console.WriteLine("Некорректный ввод, попробуйте снова.");
                    break;
            }
            Console.WriteLine("//...........................................6lab...............................................................//");


            Console.WriteLine("//...........................................6lab...............................................................//");

            Admin admin = new Admin("admin_login", "admin_password", "admin_role", "IT");
            Premium premium1 = new Premium("premium_login1", "premium_password1", "premium_role", new DateTime(2024, 2, 26));
            Premium premium2 = new Premium("premium_login2", "premium_password2", "premium_role", new DateTime(2026, 3, 26));
            Application application = new Application("Управление пользователями", 200);
            Premium user1 = new Premium();
            Admin user2 = new Admin("user2", "password2", "admin", "departament2");
            application.CreateUser("user3", "password3", "user");
            Admin user3 = new Admin("user3", "password3", "admin", "p");

            Admin user4 = new Admin("lab4", "password4", "admin", "departament4");
            Premium user5 = new Premium("lab4", "password5", "premium", new DateTime(2024, 1, 1));
            application.Add(admin);
            application.Add(premium1);
            application.Add(premium2);
            application.Add(user1);
            application.Add(user2);
            application.Add(user3);
            application.Add(user4);
            application.Add(user5);


            Application app = new Application("lab5", 10);

            Premium user6 = new Premium("lab5", "Индексатор_0", "premium", new DateTime(2025, 7, 7));
            Admin user7 = new Admin("lab5", "password7", "admin", "TT");
            Admin user8 = new Admin("lab5", "password8", "admin", "TT");
            Admin user9 = new Admin("lab5", "password9", "admin", "ИСИТ");
            Admin user10 = new Admin("lab5", "password10", "admin", "ИТ");
            Admin user11 = new Admin("lab5", "password11", "admin", "ПИ");
            Admin user12 = new Admin("lab5", "password12", "admin", "ВСИС");
            Admin user13 = new Admin("lab5", "password13", "admin", "КСИС");

            app = app + user6 + user7 + user8 + user9 + user10 + user11 + user12 + user13;

            ++app;
            app++;

            Console.WriteLine("--------------------------------------------------------------------------");
            User firstUser = app[0];
            firstUser.ShowInfo();
            Console.WriteLine("--------------------------------------------------------------------------");

            app[1] = new Premium("lab5", "Индексатор_1", "premium", new DateTime(2027, 1, 1));

            CustomNumber number = new CustomNumber(5);

            Console.WriteLine("--------------------------------------------------------------------------");
            Console.WriteLine(number.Value);
            CustomNumber qwer = number << 3;
            Console.WriteLine("Число со сдвигом: " + qwer.Value);
            Console.WriteLine("--------------------------------------------------------------------------");

            Console.WriteLine("--------------------------------------------------------------------------");
            Console.WriteLine("Сравнение объектов: ");
            Console.Write("Сравнение premium1 с premium2");
            if (premium1 < premium2)
                Console.WriteLine($"Дата подписки пользователя {premium1.GetLogin()} оформлена раньше ");
            else
                Console.WriteLine($"Дата подписки пользователя {premium2.GetLogin()} оформлена раньше ");
            Console.Write("Сравнение user5 с user6: ");
            if (user5 > user6)
                Console.WriteLine($"Дата подписки пользователя {user6.GetLogin()} оформлена раньше ");
            else
                Console.WriteLine($"Дата подписки пользователя {user5.GetLogin()} оформлена раньше ");
            Console.WriteLine("--------------------------------------------------------------------------");
            Console.WriteLine("--------------------------------------------------------------------------");
            Console.WriteLine("Сравнение объектов: ");
            Console.WriteLine("Сравнение user7 с user8");
            if (user7 == user8)
                Console.WriteLine($"Пользователи {user7.GetLogin()} и {user8.GetLogin()} имеют одинаковые логины");
            else Console.WriteLine($"Пользователи {user7.GetLogin()} и {user8.GetLogin()} имеют разные логины");

            Console.WriteLine("Сравнение user7 c user9");
            if (user7 != user9)
                Console.WriteLine($"Пользователи {user7.GetLogin()} и {user9.GetLogin()} не работают в одном отделе({user7.departament},{user9.departament})");
            else Console.WriteLine($"Пользователи {user7.GetLogin()} и {user9.GetLogin()} работают в одном отделе({user7.departament},{user9.departament})");
            Console.WriteLine("--------------------------------------------------------------------------");

            while (true)
            {
                Console.WriteLine("--------------------------------------------------------------------------");
                Console.WriteLine("1. Добавить админа.");
                Console.WriteLine("2. Удалить пользователя.");
                Console.WriteLine("3. Редактировать пользователя.");
                Console.WriteLine("4. Просмотр всех пользователей.");
                Console.WriteLine("5. Выход.");
                Console.WriteLine("6. Добавить пермиум пользователя.");
                Console.WriteLine("7. Редактировать админа.");
                Console.WriteLine("8. Редактировать премиум.");
                Console.WriteLine("9. lab5.");
                Console.WriteLine("10. lab6.");
                Console.WriteLine("--------------------------------------------------------------------------");

                string choise = Console.ReadLine();
                switch (choise)
                {
                    case "1":
                        Console.WriteLine("Введите данные админа: ");
                        Console.Write("Введите логин: ");
                        string login = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(login))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите пароль: ");
                        string password = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(password))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите роль: ");
                        string role = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(role))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите департамент: ");
                        string departament = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(departament))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Admin newAdmin = new Admin(login, role, password, departament);
                        application.Add(newAdmin);

                        break;

                    case "2":

                        Console.Write("Введите логин для удаления: ");
                        login = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(login))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }
                        application.Remove(login);
                        break;

                    case "3":

                        Console.Write("Введите логин для редактирования: ");
                        login = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(login))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите новый пароль: ");
                        password = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(password))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите новую роль: ");
                        role = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(role))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }
                        application.RedactUser(login, password, role);
                        break;

                    case "4":
                        application.ShowUser();
                        break;

                    case "5":
                        return;

                    case "6":

                        Console.WriteLine("Введите данные premium: ");
                        Console.Write("Введите логин: ");
                        login = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(login))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите пароль: ");
                        password = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(password))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите роль: ");
                        role = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(role))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Console.Write("Введите дату подписки: ");
                        string subDateInput = Console.ReadLine();
                        if (!DateTime.TryParse(subDateInput, out DateTime subDate))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Premium newPremium = new Premium(login, role, password, subDate);
                        application.Add(newPremium);

                        break;

                    case "7":
                        Console.WriteLine("Введите логин для редактирования админа: ");
                        string adminLogin = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(adminLogin))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Admin adminToEdit = null;
                        foreach (var user in application.users)
                        {
                            if (user is Admin adminUser && adminUser.GetLogin() == adminLogin)
                            {
                                adminToEdit = adminUser;
                                break;
                            }
                        }

                        if (adminToEdit == null)
                        {
                            Console.WriteLine("Пользователь не найден");
                            break;
                        }

                        Console.WriteLine("Введите новый пароль: ");
                        string adminPassword = Console.ReadLine();
                        if (!string.IsNullOrWhiteSpace(adminPassword))
                        {
                            adminToEdit.ChangePassword(adminPassword);
                        }

                        Console.WriteLine("Введите новый департамент: ");
                        string newDepartament = Console.ReadLine();
                        if (!string.IsNullOrWhiteSpace(newDepartament))
                        {
                            adminToEdit.departament = newDepartament;
                        }

                        Console.WriteLine("Введите новую роль: ");
                        string adminRole = Console.ReadLine();
                        if (!string.IsNullOrWhiteSpace(adminRole))
                        {
                            adminToEdit.ChangeRole(adminRole);
                        }

                        break;

                    case "8":
                        Console.WriteLine("Введите логин для редактирования премиум-пользователя: ");
                        string premLogin = Console.ReadLine();
                        if (string.IsNullOrWhiteSpace(premLogin))
                        {
                            Console.WriteLine("Некорректный ввод");
                            break;
                        }

                        Premium premToEdit = null;
                        foreach (var user in application.users)
                        {
                            if (user is Premium premUser && premUser.GetLogin() == premLogin)
                            {
                                premToEdit = premUser;
                                break;
                            }
                        }

                        if (premToEdit == null)
                        {
                            Console.WriteLine("Пользователь не найден");
                            break;
                        }

                        Console.WriteLine("Введите новый пароль: ");
                        string premPassword = Console.ReadLine();
                        if (!string.IsNullOrWhiteSpace(premPassword))
                        {
                            premToEdit.ChangePassword(premPassword);
                        }

                        Console.WriteLine("Введите новую дату окончания подписки: ");
                        string newDateInput = Console.ReadLine();
                        if (!string.IsNullOrWhiteSpace(newDateInput))
                        {
                            if (DateTime.TryParse(newDateInput, out DateTime newDate))
                            {
                                premToEdit.subDate = newDate;
                            }
                            else
                            {
                                Console.WriteLine("Некорректный формат даты.");
                            }
                        }

                        Console.WriteLine("Данные премиум-пользователя успешно обновлены.");
                        break;

                    case "9":
                        app.ShowUser();
                        break;

                    case "10":
                        Console.WriteLine("Массив int");
                        Console.WriteLine("--------------------------------------------------------------------------");
                        Console.Write("Введите размер массива: ");
                        int size = int.Parse(Console.ReadLine());
                        General<int> general = new General<int>(size);
                        
                        for (int i = 0; i < size; i++)
                        {
                            Console.Write($"Введите {i} элемент массива: ");
                            int value = int.Parse(Console.ReadLine());

                            
                            general.AddElement(value);
                        }
                        Console.WriteLine("Элементы массива: ");
                        general.ShowInfo();

                        Console.WriteLine("Получение элемента по индексу: ");
                        Console.Write("Введите индекс: ");
                        int index = int.Parse(Console.ReadLine());
                        Console.WriteLine("Элемент полученный по индексу: ");
                        General<int> elment = general.GetElement(index);
                        Console.WriteLine(elment.item);
                        Console.WriteLine("--------------------------------------------------------------------------");

                        Console.WriteLine("Массив char");
                        Console.WriteLine("--------------------------------------------------------------------------");
                        Console.Write("Введите размер массива: ");
                        int sizeChar = int.Parse(Console.ReadLine());
                        General<char> charGeneral = new General<char>(sizeChar);

                        for (int i = 0; i < sizeChar; i++)
                        {
                            Console.Write($"Введите {i} элемент массива: ");
                            int value = int.Parse(Console.ReadLine());


                            general.AddElement(value);
                        }
                        Console.WriteLine("Элементы массива: ");
                        general.ShowInfo();

                        Console.WriteLine("Получение элемента по индексу: ");
                        Console.Write("Введите индекс: ");
                       
                        Console.WriteLine("--------------------------------------------------------------------------");
                        break;

                    default:
                        Console.WriteLine("Некорректный ввод");
                        break;
                }

            }

        }
    }
}