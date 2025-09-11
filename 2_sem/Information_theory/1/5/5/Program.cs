using System;
using System.IO;
using System.Numerics;
using System.Text;

public class RSAAlgorithm
{
    // Расширенный алгоритм Евклида
    public static (BigInteger x, BigInteger y, BigInteger gcd) ExtendedEuclid(BigInteger a, BigInteger b)
    {
        BigInteger d0 = a, d1 = b;
        BigInteger x0 = 1, x1 = 0;
        BigInteger y0 = 0, y1 = 1;

        while (d1 > 1)
        {
            BigInteger q = d0 / d1;
            BigInteger d2 = d0 % d1;
            BigInteger x2 = x0 - q * x1;
            BigInteger y2 = y0 - q * y1;

            d0 = d1;
            d1 = d2;
            x0 = x1;
            x1 = x2;
            y0 = y1;
            y1 = y2;
        }

        return (x1, y1, d1);
    }

    // Вычисление мультипликативного обратного по модулю
    public static BigInteger ModInverse(BigInteger a, BigInteger m)
    {
        var (x, y, gcd) = ExtendedEuclid(m, a);
        if (y < 0)
            y += m;
        return y;
    }

    // Функция Эйлера для n = p*q
    public static BigInteger EulerFunction(BigInteger p, BigInteger q)
    {
        return (p - 1) * (q - 1);
    }

    // Алгоритм быстрого возведения в степень по модулю
    public static BigInteger FastExp(BigInteger a, BigInteger z, BigInteger n)
    {
        BigInteger a1 = a;
        BigInteger z1 = z;
        BigInteger x = 1;

        while (z1 > 0)
        {
            while (z1 % 2 == 0)
            {
                z1 /= 2;
                a1 = (a1 * a1) % n;
            }

            z1 -= 1;
            x = (x * a1) % n;
        }

        return x;
    }

    // Проверка, что числа взаимно простые
    public static bool AreCoprime(BigInteger a, BigInteger b)
    {
        return BigInteger.GreatestCommonDivisor(a, b) == 1;
    }

    // Проверка на простоту (упрощенная версия)
    public static bool IsPrime(BigInteger number)
    {
        if (number < 2) return false;
        if (number == 2) return true;
        if (number % 2 == 0) return false;

        BigInteger boundary = BigIntegerSqrt(number);

        for (BigInteger i = 3; i <= boundary; i += 2)
        {
            if (number % i == 0)
                return false;
        }

        return true;
    }

    // Квадратный корень для BigInteger (теперь public)
    public static BigInteger BigIntegerSqrt(BigInteger n)
    {
        if (n == 0) return 0;
        if (n > 0)
        {
            int bitLength = Convert.ToInt32(Math.Ceiling(BigInteger.Log(n, 2)));
            BigInteger root = BigInteger.One << (bitLength / 2);

            while (!IsSqrt(n, root))
            {
                root += n / root;
                root /= 2;
            }

            return root;
        }

        throw new ArithmeticException("NaN");
    }

    private static bool IsSqrt(BigInteger n, BigInteger root)
    {
        BigInteger lowerBound = root * root;
        BigInteger upperBound = (root + 1) * (root + 1);
        return (n >= lowerBound && n < upperBound);
    }
}

public class RSAEncryptor
{
    public static (BigInteger ko, BigInteger r) EncryptFile(string inputFile, string outputFile, BigInteger p, BigInteger q, BigInteger kc)
    {
        // Проверка ограничений
        if (p == q)
            throw new ArgumentException("p и q должны быть разными простыми числами");

        if (!RSAAlgorithm.IsPrime(p) || !RSAAlgorithm.IsPrime(q))
            throw new ArgumentException("p и q должны быть простыми числами");

        // Вычисление параметров RSA
        BigInteger r = p * q;
        BigInteger phi_r = RSAAlgorithm.EulerFunction(p, q);

        // Проверка, что kc и phi_r взаимно простые
        if (!RSAAlgorithm.AreCoprime(kc, phi_r))
            throw new ArgumentException("KC и φ(r) должны быть взаимно простыми");

        // Вычисление открытого ключа KO
        BigInteger ko = RSAAlgorithm.ModInverse(kc, phi_r);

        Console.WriteLine("Параметры RSA:");
        Console.WriteLine($"p = {p}, q = {q}");
        Console.WriteLine($"r = {r}");
        Console.WriteLine($"φ(r) = {phi_r}");
        Console.WriteLine($"Закрытый ключ KC = {kc}");
        Console.WriteLine($"Открытый ключ KO = {ko}");

        // Шифрование файла побайтово
        using (FileStream fsIn = new FileStream(inputFile, FileMode.Open, FileAccess.Read))
        using (FileStream fsOut = new FileStream(outputFile, FileMode.Create, FileAccess.Write))
        using (BinaryWriter writer = new BinaryWriter(fsOut))
        {
            byte[] buffer = new byte[1];
            int bytesRead;

            while ((bytesRead = fsIn.Read(buffer, 0, 1)) > 0)
            {
                // Преобразование байта в число
                BigInteger m = buffer[0];

                // Шифрование по RSA
                BigInteger c = RSAAlgorithm.FastExp(m, ko, r);

                // Запись зашифрованного значения (2 байта)
                byte[] encryptedBytes = c.ToByteArray();

                // Обеспечиваем ровно 2 байта
                byte[] outputBytes = new byte[2];
                if (encryptedBytes.Length > 2)
                {
                    // Берем младшие 2 байта
                    Array.Copy(encryptedBytes, 0, outputBytes, 0, 2);
                }
                else if (encryptedBytes.Length < 2)
                {
                    // Дополняем нулями слева
                    Array.Copy(encryptedBytes, 0, outputBytes, 2 - encryptedBytes.Length, encryptedBytes.Length);
                }
                else
                {
                    outputBytes = encryptedBytes;
                }

                writer.Write(outputBytes);
            }
        }

        Console.WriteLine($"Файл '{inputFile}' зашифрован и сохранен как '{outputFile}'");
        return (ko, r);
    }
}

public class RSADecryptor
{
    public static void DecryptFile(string inputFile, string outputFile, BigInteger r, BigInteger kc)
    {
        // Проверка, что файл имеет правильный размер (кратен 2 байтам)
        FileInfo fileInfo = new FileInfo(inputFile);
        if (fileInfo.Length % 2 != 0)
            throw new ArgumentException("Размер зашифрованного файла должен быть кратен 2 байтам");

        // Расшифрование файла
        using (FileStream fsIn = new FileStream(inputFile, FileMode.Open, FileAccess.Read))
        using (FileStream fsOut = new FileStream(outputFile, FileMode.Create, FileAccess.Write))
        using (BinaryReader reader = new BinaryReader(fsIn))
        {
            byte[] buffer = new byte[2];

            while (reader.Read(buffer, 0, 2) > 0)
            {
                // Преобразование байтов в число
                BigInteger c = new BigInteger(buffer);

                // Расшифрование по RSA
                BigInteger m = RSAAlgorithm.FastExp(c, kc, r);

                // Проверка, что результат - валидный байт
                if (m < 0 || m > 255)
                    throw new InvalidOperationException($"Некорректное расшифрованное значение: {m}");

                // Запись расшифрованного байта
                fsOut.WriteByte((byte)m);
            }
        }

        Console.WriteLine($"Файл '{inputFile}' расшифрован и сохранен как '{outputFile}'");
    }
}

public class RSACracker
{
    public static void CrackFile(string inputFile, string outputFile, BigInteger r, BigInteger ko)
    {
        // Факторизация r для нахождения p и q
        var (p, q) = Factorize(r);

        // Вычисление функции Эйлера
        BigInteger phi_r = RSAAlgorithm.EulerFunction(p, q);

        // Вычисление закрытого ключа KC
        BigInteger kc = RSAAlgorithm.ModInverse(ko, phi_r);

        Console.WriteLine("Взлом завершен:");
        Console.WriteLine($"Найдены p = {p}, q = {q}");
        Console.WriteLine($"φ(r) = {phi_r}");
        Console.WriteLine($"Закрытый ключ KC = {kc}");

        // Используем найденный ключ для расшифрования
        RSADecryptor.DecryptFile(inputFile, outputFile, r, kc);
    }

    // Простая функция факторизации (для учебных целей)
    private static (BigInteger p, BigInteger q) Factorize(BigInteger n)
    {
        if (n % 2 == 0)
            return (2, n / 2);

        // Для больших чисел это неэффективно, но для учебного примера подойдет
        BigInteger sqrt = RSAAlgorithm.BigIntegerSqrt(n);

        for (BigInteger i = 3; i <= sqrt; i += 2)
        {
            if (n % i == 0)
                return (i, n / i);
        }

        throw new ArgumentException($"Не удалось факторизовать {n}");
    }
}

class Program
{
    static void Main(string[] args)
    {
        Console.OutputEncoding = Encoding.UTF8;
        Console.WriteLine("RSA Cryptosystem - Практикум");
        Console.WriteLine("1. Шифрование файла");
        Console.WriteLine("2. Расшифрование файла");
        Console.WriteLine("3. Взлом файла");
        Console.WriteLine("4. Пример из учебного материала");
        Console.WriteLine("5. Выход");

        while (true)
        {
            Console.Write("\nВыберите операцию (1-5): ");
            string choice = Console.ReadLine();

            try
            {
                switch (choice)
                {
                    case "1":
                        EncryptOperation();
                        break;

                    case "2":
                        DecryptOperation();
                        break;

                    case "3":
                        CrackOperation();
                        break;

                    case "4":
                        TextbookExample();
                        break;

                    case "5":
                        Console.WriteLine("Выход из программы...");
                        return;

                    default:
                        Console.WriteLine("Неверный выбор. Попробуйте снова.");
                        break;
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Ошибка: {ex.Message}");
            }
        }
    }

    static void EncryptOperation()
    {
        Console.Write("Входной файл: ");
        string inputFile = Console.ReadLine();

        Console.Write("Выходной файл: ");
        string outputFile = Console.ReadLine();

        Console.Write("p: ");
        BigInteger p = BigInteger.Parse(Console.ReadLine());

        Console.Write("q: ");
        BigInteger q = BigInteger.Parse(Console.ReadLine());

        Console.Write("Закрытый ключ KC: ");
        BigInteger kc = BigInteger.Parse(Console.ReadLine());

        var (ko, r) = RSAEncryptor.EncryptFile(inputFile, outputFile, p, q, kc);
        Console.WriteLine($"Открытый ключ: KO = {ko}, r = {r}");
    }

    static void DecryptOperation()
    {
        Console.Write("Зашифрованный файл: ");
        string inputFile = Console.ReadLine();

        Console.Write("Выходной файл: ");
        string outputFile = Console.ReadLine();

        Console.Write("Модуль r: ");
        BigInteger r = BigInteger.Parse(Console.ReadLine());

        Console.Write("Закрытый ключ KC: ");
        BigInteger kc = BigInteger.Parse(Console.ReadLine());

        RSADecryptor.DecryptFile(inputFile, outputFile, r, kc);
    }

    static void CrackOperation()
    {
        Console.Write("Зашифрованный файл: ");
        string inputFile = Console.ReadLine();

        Console.Write("Выходной файл: ");
        string outputFile = Console.ReadLine();

        Console.Write("Модуль r: ");
        BigInteger r = BigInteger.Parse(Console.ReadLine());

        Console.Write("Открытый ключ KO: ");
        BigInteger ko = BigInteger.Parse(Console.ReadLine());

        RSACracker.CrackFile(inputFile, outputFile, r, ko);
    }

    // Пример из учебного материала (строка "BSUIR")
    static void TextbookExample()
    {
        Console.WriteLine("=== Пример из учебного материала ===");

        // Создаем тестовый файл
        string inputFile = "test_bsuir.txt";
        File.WriteAllText(inputFile, "BSUIR", Encoding.ASCII);

        // Параметры из примера
        BigInteger p = 41;
        BigInteger q = 59;
        BigInteger kc = 133;

        Console.WriteLine("Шифрование строки 'BSUIR'...");
        var (ko, r) = RSAEncryptor.EncryptFile(inputFile, "encrypted_bsuir.bin", p, q, kc);

        Console.WriteLine("\nРасшифрование...");
        RSADecryptor.DecryptFile("encrypted_bsuir.bin", "decrypted_bsuir.txt", r, kc);

        Console.WriteLine("\nВзлом...");
        RSACracker.CrackFile("encrypted_bsuir.bin", "cracked_bsuir.txt", r, ko);

        // Показываем результаты
        string decrypted = File.ReadAllText("decrypted_bsuir.txt", Encoding.ASCII);
        string cracked = File.ReadAllText("cracked_bsuir.txt", Encoding.ASCII);

        Console.WriteLine($"Исходный текст: BSUIR");
        Console.WriteLine($"Расшифрованный: {decrypted}");
        Console.WriteLine($"Взломанный: {cracked}");

        // Очистка временных файлов
        File.Delete(inputFile);
        File.Delete("encrypted_bsuir.bin");
        File.Delete("decrypted_bsuir.txt");
        File.Delete("cracked_bsuir.txt");
    }
}