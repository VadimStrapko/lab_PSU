using System.Text;
using System.Text.RegularExpressions;

public class FrequencyTables
{
    public static readonly Dictionary<char, double> EnglishFrequencies = new Dictionary<char, double>
    {
        {'a', 8.167}, {'b', 1.492}, {'c', 2.782}, {'d', 4.253}, {'e', 12.702},
        {'f', 2.228}, {'g', 2.015}, {'h', 6.094}, {'i', 6.966}, {'j', 0.153},
        {'k', 0.772}, {'l', 4.025}, {'m', 2.406}, {'n', 6.749}, {'o', 7.507},
        {'p', 1.929}, {'q', 0.095}, {'r', 5.987}, {'s', 6.327}, {'t', 9.056},
        {'u', 2.758}, {'v', 0.978}, {'w', 2.360}, {'x', 0.150}, {'y', 1.974},
        {'z', 0.074}
    };
}

public class VigenereCipher
{
    public static string Encrypt(string plaintext, string key)
    {
        plaintext = PreprocessText(plaintext);
        key = PreprocessText(key);

        StringBuilder ciphertext = new StringBuilder();
        int keyIndex = 0;

        foreach (char c in plaintext)
        {
            if (char.IsLetter(c))
            {
                char baseChar = 'a';
                int alphabetSize = 26;

                int plainChar = c - baseChar;
                int keyChar = key[keyIndex % key.Length] - baseChar;
                int encryptedChar = (plainChar + keyChar) % alphabetSize;

                ciphertext.Append((char)(encryptedChar + baseChar));
                keyIndex++;
            }
            else
            {
                ciphertext.Append(c);
            }
        }

        return ciphertext.ToString();
    }

    public static string Decrypt(string ciphertext, string key)
    {
        ciphertext = PreprocessText(ciphertext);
        key = PreprocessText(key);

        StringBuilder plaintext = new StringBuilder();
        int keyIndex = 0;

        foreach (char c in ciphertext)
        {
            if (char.IsLetter(c))
            {
                char baseChar = 'a';
                int alphabetSize = 26;

                int cipherChar = c - baseChar;
                int keyChar = key[keyIndex % key.Length] - baseChar;
                int decryptedChar = (cipherChar - keyChar + alphabetSize) % alphabetSize;

                plaintext.Append((char)(decryptedChar + baseChar));
                keyIndex++;
            }
            else
            {
                plaintext.Append(c);
            }
        }
        return plaintext.ToString();
    }

    private static string PreprocessText(string text)
    {
        text = text.ToLower();
        text = Regex.Replace(text, @"[^a-z]", "");
        return text;
    }
}

public class KasiskiAnalysis
{
    public static int FindKeyLength(string ciphertext, int maxNgramLength = 5, int minOccurrences = 3)
    {
        var distances = new List<int>();

        for (int n = 3; n <= maxNgramLength; n++)
        {
            var ngramPositions = FindRepeatingNgrams(ciphertext, n, minOccurrences);

            foreach (var ngram in ngramPositions)
            {
                var positions = ngram.Value;
                for (int i = 0; i < positions.Count - 1; i++)
                {
                    for (int j = i + 1; j < positions.Count; j++)
                    {
                        int distance = positions[j] - positions[i];
                        if (distance > 0)
                        {
                            distances.Add(distance);
                        }
                    }
                }
            }
        }

        if (distances.Count == 0)
            return -1;

        return FindMostCommonGCD(distances);
    }
    //1-2)                    n-грамма позиция
    private static Dictionary<string, List<int>> FindRepeatingNgrams(string text, int n, int minRepeat)
    {
        var ngrams = new Dictionary<string, List<int>>();

        for (int i = 0; i <= text.Length - n; i++)
        {
            string ngram = text.Substring(i, n);

            if (ngrams.ContainsKey(ngram))
            {
                ngrams[ngram].Add(i);
            }
            else
            {
                ngrams[ngram] = new List<int> { i };
            }
        }

        return ngrams.Where(kvp => kvp.Value.Count >= minRepeat)
                    .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
    }
    //3-4)
    private static int FindMostCommonGCD(List<int> numbers)
    {
        //                            НОД   кол-во
        var gcdCounts = new Dictionary<int, int>();

        for (int i = 0; i < numbers.Count; i++)
        {
            for (int j = i + 1; j < numbers.Count; j++)
            {
                int gcd = GCD(numbers[i], numbers[j]);
                if (gcd > 1)
                {
                    if (gcdCounts.ContainsKey(gcd))
                        gcdCounts[gcd]++;
                    else
                        gcdCounts[gcd] = 1;
                }
            }
        }

        if (gcdCounts.Count == 0)
            return -1;

        return gcdCounts.OrderByDescending(kvp => kvp.Value).First().Key;
    }

    private static int GCD(int a, int b)
    {
        while (b != 0)
        {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
    //5)
    public static string FindKey(string ciphertext, int keyLength)
    {
        StringBuilder key = new StringBuilder();
        var frequencies = FrequencyTables.EnglishFrequencies;

        int alphabetSize = 26;
        char baseChar = 'a';

        for (int i = 0; i < keyLength; i++)
        {
            string column = GetColumn(ciphertext, i, keyLength);
            int shift = FindBestShift(column, frequencies, baseChar, alphabetSize);
            key.Append((char)(baseChar + shift));
        }

        return key.ToString();
    }
    //6
    private static string GetColumn(string text, int start, int step)
    {
        StringBuilder column = new StringBuilder();
        for (int i = start; i < text.Length; i += step)
        {
            if (char.IsLetter(text[i]))
                column.Append(text[i]);
        }
        return column.ToString();
    }

    private static int FindBestShift(string text, Dictionary<char, double> expectedFrequencies, char baseChar, int alphabetSize)
    {
        double bestScore = double.MinValue;
        int bestShift = 0;

        for (int shift = 0; shift < alphabetSize; shift++)
        {
            double score = CalculateFrequencyScore(text, shift, expectedFrequencies, baseChar, alphabetSize);
            if (score > bestScore)
            {
                bestScore = score;
                bestShift = shift;
            }
        }

        return bestShift;
    }

    private static double CalculateFrequencyScore(string text, int shift, Dictionary<char, double> expectedFrequencies, char baseChar, 
        int alphabetSize)
    {
        var actualFrequencies = new Dictionary<char, int>();
        int totalLetters = 0;

        foreach (char c in text)
        {
            if (char.IsLetter(c))
            {
                int shiftedChar = (c - baseChar - shift + alphabetSize) % alphabetSize;
                char decryptedChar = (char)(baseChar + shiftedChar);

                if (actualFrequencies.ContainsKey(decryptedChar))
                    actualFrequencies[decryptedChar]++;
                else
                    actualFrequencies[decryptedChar] = 1;

                totalLetters++;
            }
        }

        if (totalLetters == 0) return 0;

        double score = 0;
        foreach (var kvp in expectedFrequencies)
        {
            double expectedFrequency = kvp.Value / 100;
            double actualFrequency = actualFrequencies.ContainsKey(kvp.Key) ?
                (double)actualFrequencies[kvp.Key] / totalLetters : 0;

            score += expectedFrequency * actualFrequency;
        }

        return score;
    }
}

public class TextUtilities
{
    public static string CleanText(string text)
    {
        text = text.ToLower();
        text = Regex.Replace(text, @"[^a-z\s]", "");
        text = Regex.Replace(text, @"\s+", " ");
        return text.Trim();
    }
}

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Инструмент криптоанализа методом Касиски");
        Console.WriteLine("========================================");

        try
        {
            if (!File.Exists("encrypt.txt"))
            {
                Console.WriteLine("Файл encrypt.txt не найден. Создайте файл с исходным текстом.");
                return;
            }

            string originalText = File.ReadAllText("encrypt.txt");
            string cleanedText = TextUtilities.CleanText(originalText);

            Console.WriteLine("Исходный текст прочитан из encrypt.txt");
            Console.WriteLine("Длина текста: " + cleanedText.Length + " символов");

            Console.Write("Введите ключ для шифрования: ");
            string key = Console.ReadLine();

            if (string.IsNullOrEmpty(key))
            {
                Console.WriteLine("Ключ не может быть пустым.");
                return;
            }

            string ciphertext = VigenereCipher.Encrypt(cleanedText, key);

            File.WriteAllText("decrypt.txt", ciphertext);
            Console.WriteLine("Зашифрованный текст сохранен в decrypt.txt");

            Console.WriteLine("\nПроводим анализ методом Касиски...");
            int foundKeyLength = KasiskiAnalysis.FindKeyLength(ciphertext);

            string resultText = "Результаты анализа методом Касиски:\n";
            resultText += "========================================\n";
            resultText += $"Длина исходного текста: {cleanedText.Length} символов\n";
            resultText += $"Использованный ключ: {key}\n";
            resultText += $"Длина ключа: {key.Length}\n";
            resultText += $"Найденная длина ключа: {foundKeyLength}\n";

            if (foundKeyLength > 0)
            {
                string foundKey = KasiskiAnalysis.FindKey(ciphertext, foundKeyLength);
                resultText += $"Найденный ключ: {foundKey}\n";

                string decryptedText = VigenereCipher.Decrypt(ciphertext, foundKey);
                resultText += $"Ключ верный: {(foundKey == key ? "ДА" : "НЕТ")}\n";

                if (foundKey == key)
                {
                    resultText += "Анализ успешен!\n";
                }
                else
                {
                    resultText += "Анализ не смог найти правильный ключ.\n";
                }
            }
            else
            {
                resultText += "Не удалось определить длину ключа.\n";
            }

            File.WriteAllText("result.txt", resultText);
            Console.WriteLine("Результаты анализа сохранены в result.txt");

            Console.WriteLine("\n" + resultText);

        }
        catch (Exception ex)
        {
            Console.WriteLine("Произошла ошибка: " + ex.Message);
        }

        Console.WriteLine("\nНажмите любую клавишу для выхода...");
        Console.ReadKey();
    }
}