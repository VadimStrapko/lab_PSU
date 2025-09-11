using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace Lab4
{
    class Program
    {
        static void Main(string[] args)
        {
            string inputPath = @"C:\Users\admin\Desktop\IT\4\4\bin\Debug\net8.0\input.txt";
            string decodedPath = @"C:\Users\admin\Desktop\IT\4\4\bin\Debug\net8.0\output.txt";
            string text = "";
            if (File.Exists(inputPath))
                text = File.ReadAllText(inputPath);
            else
                Console.WriteLine("Файл не найден!");

            //генерация ключей
            var keysForEncode = KeysForEncode();
            var changedDataForEncode = Round(TextToBinary(text), keysForEncode);
            var changedDataForDecode = Round(changedDataForEncode, KeysForDecode(keysForEncode));
            var encodedText = BinToString(Round(TextToBinary(text), keysForEncode));
            var decodedText = BinToString(changedDataForDecode);
            File.WriteAllText(decodedPath, decodedText);

            Console.WriteLine(String.Format(" Text:\n {0}\n\n\n Encoded:\n {1}\n\n\n Decoded:\n {2}", text, encodedText, decodedText));
        }

        static List<int> KeysForDecode(List<int> Keys)
        {
            List<int> DecodeKeys = new List<int>();
            for (int i = 0; i < 52; i++)
                DecodeKeys.Add(0);

            // ModInverse) для Keys[48] и Keys[51] по модулю 2^16+1 для 49 и 50 - 2^16
            DecodeKeys[0] = MULT_INV(Keys[48], (Convert.ToInt32(Math.Pow(2, 16)) + 1)); // противоположно *
            DecodeKeys[1] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[49];                  // отменим результат +
            DecodeKeys[2] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[50];
            DecodeKeys[3] = MULT_INV(Keys[51], (Convert.ToInt32(Math.Pow(2, 16)) + 1));

            for (int i = 0; i < 47; i += 6) // шаг 6 ну понятно ключей исп. 6 типо
            {
                DecodeKeys[4 + i] = Keys[46 - i]; // поменяли типо значения
                DecodeKeys[5 + i] = Keys[47 - i];
                DecodeKeys[6 + i] = MULT_INV(Keys[42 - i], (Convert.ToInt32(Math.Pow(2, 16)) + 1));// просто * обратное
                if (i != 42)
                { // это ифище для просмотра последние ли действия чтобы понять надо переставлять или нет уже наши +
                    DecodeKeys[7 + i] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[44 - i];//аддитивная
                    DecodeKeys[8 + i] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[43 - i];
                }
                else
                {
                    DecodeKeys[7 + i] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[43 - i];
                    DecodeKeys[8 + i] = Convert.ToInt32(Math.Pow(2, 16)) - Keys[44 - i];
                }
                DecodeKeys[9 + i] = MULT_INV(Keys[45 - i], (Convert.ToInt32(Math.Pow(2, 16)) + 1)); // просто * обратное
            }
            return DecodeKeys;
        }

        static int MULT_INV(int a, int mod)
        {
            int q = 0, temp = 0, x1 = 0, x2 = 1, mod0 = mod;
            while (a > 1)
            {
                q = a / mod;
                temp = mod;
                mod = a % mod;
                a = temp;
                temp = x1;
                x1 = x2 - q * x1;
                x2 = temp;
            }
            if (x2 < 0)
                x2 = (x2 % mod0 + mod0) % mod0;
            return x2;
        }

        static List<int> KeysForEncode()
        {
            Console.WriteLine("enter the key ");
            string key = "" + Console.ReadLine();

            string binarykey = "";
            List<int> Keys = new List<int>();
            foreach (var symbol in key)
            {
                binarykey += Convert.ToString(Convert.ToInt32(symbol), 2).PadLeft(8, '0');
            }

            for (int i = 0; i < 7; i++)
            {
                for (int j = 0; j < 8; j++)
                {
                    if (Keys.Count < 52)
                    {
                        //берем первые 16 бит
                        Keys.Add(Convert.ToInt32(binarykey.Substring(j * 16, 16), 2));
                    }
                    else { break; }
                }
                //сдвиг 25 влево
                binarykey = binarykey.Substring(25) + binarykey.Substring(0, 25);
            }
            return Keys;
        }

        static int Sum(int n1, int n2)//+(2^16)
        {
            return (n1 + n2) % Convert.ToInt32(Math.Pow(2, 16));
        }

        static int Muiltiply(int n1, int n2)//*(2^16+1)
        {
            int n;
            if (n1 == 0)
                n1 = Convert.ToInt32(Math.Pow(2, 16));
            if (n2 == 0)
                n2 = Convert.ToInt32(Math.Pow(2, 16));
            n = (int)((uint)(n1 * n2) % (Convert.ToInt32(Math.Pow(2, 16)) + 1));
            if (Convert.ToString(n, 2).Length > 16)
                return Convert.ToInt32(Convert.ToString(n, 2).Substring(1));
            else return n;
        }

        static List<int> Round(List<int> sublist, List<int> Keys)
        {
            List<int> newsublist = new List<int>();
            for (int i = 0; sublist.Count != 0; i++)
            {
                List<int> k = new List<int>();
                foreach (var key in Keys)
                {
                    k.Add(key);
                }
                //операции над подблоками(4)
                for (int j = 0; j < 8; j++)
                {
                    sublist[0] = Muiltiply(sublist[0], k[0]);
                    sublist[1] = Sum(sublist[1], k[1]);
                    sublist[2] = Sum(sublist[2], k[2]);
                    sublist[3] = Muiltiply(sublist[3], k[3]);

                    int e = sublist[0] ^ sublist[2];
                    int f = sublist[1] ^ sublist[3];

                    e = Muiltiply(e, k[4]);
                    f = Sum(f, e);
                    f = Muiltiply(f, k[5]);
                    e = Sum(e, f);

                    sublist[0] = sublist[0] ^ f;
                    sublist[1] = sublist[1] ^ e;
                    sublist[2] = sublist[2] ^ f;
                    sublist[3] = sublist[3] ^ e;

                    if (j != 7)
                    {
                        var temp = sublist[1];
                        sublist[1] = sublist[2];
                        sublist[2] = temp;
                    }

                    k.RemoveRange(0, 6);
                }
                //9 раунд
                sublist[0] = Muiltiply(sublist[0], k[0]);
                sublist[1] = Sum(sublist[1], k[1]);
                sublist[2] = Sum(sublist[2], k[2]);
                sublist[3] = Muiltiply(sublist[3], k[3]);

                k.RemoveRange(0, 4);
                for (int j = 0; j < 4; j++)
                    newsublist.Add(sublist[j]);

                sublist.RemoveRange(0, 4);
            }
            return newsublist;
        }

        static List<int> TextToBinary(string text)
        {
            
            byte[] bytes = Encoding.UTF8.GetBytes(text);

            int paddingLength = 8 - (bytes.Length % 8);//кол-во байтов для доп
            if (paddingLength == 0) paddingLength = 8;

            byte[] paddedBytes = new byte[bytes.Length + paddingLength];
            Array.Copy(bytes, paddedBytes, bytes.Length);

            for (int i = bytes.Length; i < paddedBytes.Length; i++)
            {
                paddedBytes[i] = (byte)paddingLength;
            }

            string binarytext = "";
            foreach (byte b in paddedBytes)
            {
                binarytext += Convert.ToString(b, 2).PadLeft(8, '0');
            }

            List<string> list = new List<string>();
            for (int i = 0; i < binarytext.Length; i += 64)
            {
                list.Add(binarytext.Substring(i, 64));
            }

            List<int> sublist = new List<int>();
            foreach (var str in list)
            {
                for (int i = 0; i < str.Length; i += 16)
                {
                    sublist.Add(Convert.ToInt32(str.Substring(i, 16), 2));
                }
            }
            return sublist;
        }

        static string BinToString(List<int> sublist)
        {
            string binaryText = "";
            foreach (var num in sublist)
            {
                binaryText += Convert.ToString(num, 2).PadLeft(16, '0');
            }

            List<byte> byteList = new List<byte>();
            for (int i = 0; i < binaryText.Length; i += 8)
            {
                if (i + 8 <= binaryText.Length)
                {
                    byteList.Add(Convert.ToByte(binaryText.Substring(i, 8), 2));
                }
            }

            //удаляем PKCS#7 padding
            byte[] bytes = byteList.ToArray();

            if (bytes.Length == 0)
                return string.Empty;

            int paddingLength = bytes[bytes.Length - 1];
            
            //проверка на корректность
            if (paddingLength > 0 && paddingLength <= 8 && bytes.Length >= paddingLength)
            {
                bool validPadding = true;
                for (int i = bytes.Length - paddingLength; i < bytes.Length; i++)
                {
                    if (bytes[i] != paddingLength)
                    {
                        validPadding = false;
                        break;
                    }
                }

                if (validPadding)
                {
                    //уаляем padding
                    byte[] resultBytes = new byte[bytes.Length - paddingLength];
                    Array.Copy(bytes, resultBytes, resultBytes.Length);
                    return Encoding.UTF8.GetString(resultBytes);
                }
            }

            //если файл поврежден
            return Encoding.UTF8.GetString(bytes);
        }
    }
}