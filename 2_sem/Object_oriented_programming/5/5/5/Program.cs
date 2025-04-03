
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

class Admin : User
{
    public string departament;

    public Admin(string login, string password, string role, string departament) : base(login, password, role)
    {
        this.departament = departament;
        Console.WriteLine("Конструктор класса Admin с параметрами вызван");
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

class Premium : User
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
                throw new IndexOutOfRangeException("Некорректный ввод");
        }
        set
        {
            if (index >= 0 && index < users.Count)
                users[index] = value;
            else
                throw new IndexOutOfRangeException("Некорректный ввод");
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
            if(premium1 < premium2)
                            Console.WriteLine($"Дата подписки пользователя {premium1.GetLogin()} оформлена раньше ");
            else 
                Console.WriteLine($"Дата подписки пользователя {premium2.GetLogin()} оформлена раньше ");
            Console.Write("Сравнение user5 с user6: ");
            if(user5 > user6)
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
            if(user7 != user9)
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

                    default:
                        Console.WriteLine("Некорректный ввод");
                        break;
                }

            }

        }
    }
}