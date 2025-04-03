class User
{
    private string login;
    private string password;
    private string role;

    public User(string login, string password, string role)
    {
        this.login = login;
        this.password = password;
        this.role = role;
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

    public void ChangePassword(string newPassword, string confirmPassword)
    {
        if (newPassword == confirmPassword)
        {
            password = newPassword;
        }
        else
        {
            Console.WriteLine("Пароли не совпадают.");
        }
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

    public void Add(User user)
    {
        users.Add(user);
    }

    public void Remove(string login)
    {
        users.RemoveAll(u => u.GetLogin() == login);
    }

    public void RedactUser(string login, string newPassword, string confirmPassword, string newRole)
    {
        foreach (var user in users)
        {
            if (user.GetLogin() == login)
            {
                user.ChangePassword(newPassword, confirmPassword); 
                user.ChangeRole(newRole);
                return;
            }
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
        Application application = new Application("Управление пользователями", 2);

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
                    Console.Write("Подтвердите новый пароль: ");
                    string confirmPassword = Console.ReadLine(); 
                    if (string.IsNullOrWhiteSpace(confirmPassword))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    if (password == confirmPassword)
                    {
                        password = confirmPassword;
                    }
                    else
                    {
                        Console.WriteLine("Пароли не совпадают.");
                    }
                    Console.Write("Введите новую роль: ");
                    role = Console.ReadLine();
                    if (string.IsNullOrWhiteSpace(role))
                    {
                        Console.WriteLine("Некорректный ввод.");
                        break;
                    }
                    application.RedactUser(login, password, confirmPassword, role); 
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