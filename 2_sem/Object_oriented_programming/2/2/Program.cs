class User
{
    private string login;
    private string password;
    private string role;

    public User(string login, string password, string role)
    {
        Console.WriteLine("Конструктор с параметрами вызван.");
        this.login = login;
        this.password = password;
        this.role = role;
    }

    public User()
    {
        Console.WriteLine("Конструктор без параметров вызван.");
    }

    public User(User user)
    {
        Console.WriteLine("Конструктор копирования вызван.");
        this.login = user.login;
        this.password = user.password;
        this.role = user.role;
    }

    public void MoveFrom(User user)
    {
        Console.WriteLine("Метод перемещения вызван.");
        this.login = user.login;
        this.password = user.password;
        this.role = user.role;

        user.login = null;
        user.password = null;
        user.role = null;
    }

    ~User()
    {
        Console.WriteLine("Деструктор вызван.");
    }


    public void ShowInfo()
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

    public Application()
    {
        this.name = "Default Application";
        this.volume = 0;
        this.users = new List<User>();
    }

    public Application(Application other)
    {
        this.name = other.name;
        this.volume = other.volume;
        this.users = new List<User>();

        foreach (var user in other.users)
        {
            this.users.Add(new User(user)); 
        }
    }

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

    public void CreateUser(string login, string password, string role)
    {
        User user = new User(login, password, role);
        Add(user);
    }

    public void CreateCopies(User user, int count)
    {
        for (int i = 0; i < count; i++)
        {
            User copy = new User(user);
            Add(copy);
        }
    }

    public void ShowUser()
    {
        foreach (var user in users)
        {
            user.ShowInfo();
        }
    }
}

class Program
{
    static void Main()
    {
        Application application = new Application("Управление пользователями", 200);

        User user1 = new User();
        User user2 = new User("user2", "password2", "admin");
        application.Add(user2);

        application.CreateUser("user3", "password3", "user");

        application.CreateCopies(user2, 3);

        User user3 = new User("user3", "password3", "admin");
        User user4 = new User(user3); 
        user4.ShowInfo(); 
        user3.ShowInfo();

      

        while (true)
        {
            Console.WriteLine("------------------------------------------------------");
            Console.WriteLine("1. Добавить пользователя.");
            Console.WriteLine("2. Удалить пользователя.");
            Console.WriteLine("3. Редактировать пользователя.");
            Console.WriteLine("4. Просмотр всех пользователей.");
            Console.WriteLine("5. Выход.");
            Console.WriteLine("------------------------------------------------------");

            string choise = Console.ReadLine();
            switch (choise)
            {
                case "1":
                    Console.Write("Введите логин: ");
                    string login = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(login))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    Console.Write("Введите пароль: ");
                    string password = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(password))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    Console.Write("Введите роль: ");
                    string role = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(role))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    application.Add(new User(login, password, role));
                    break;

                case "2":
                    Console.Write("Введите логин для удаления: ");
                    login = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(login))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    application.Remove(login);
                    break;

                case "3":
                    Console.Write("Введите логин для редактирования: ");
                    login = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(login))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    Console.Write("Введите новый пароль: ");
                    password = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(password))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    Console.Write("Введите новую роль: ");
                    role = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(role))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    application.RedactUser(login, password, role);
                    break;

                case "4":
                    application.ShowUser();
                    break;

                case "5":
                    return;


                default:
                    Console.WriteLine("Некорректный ввод.");
                    break;
            }

        }

    }
}