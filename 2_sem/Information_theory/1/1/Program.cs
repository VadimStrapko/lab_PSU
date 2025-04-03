namespace Huffman
{
    public struct Table_Item
    {
        public int num_friq;
        public char letter;
        public Table_Item()
        {
            letter = '\0';
        }
    }

    public class Tree
    {
        public Tree right;
        public Tree left;
        public Table_Item core;
        public int core_friq = 0;
        public int core_pos = 0;
    }

    class Program
    {
        public static string Decode(string encodedString, Dictionary<string, char> reverseCodeTable)
        {
            string currentCode = "";
            string decodedOutput = "";

            foreach (char bit in encodedString)
            {
                currentCode += bit;

                if (reverseCodeTable.ContainsKey(currentCode))
                {
                    decodedOutput += reverseCodeTable[currentCode];
                    currentCode = "";
                }
            }

            return decodedOutput;
        }

        public static void Code(Tree Core, Dictionary<char, string> Table, string s)
        {
            if (Core == null)
            {
                return;
            }

            if (Core.left == null && Core.right == null)
            {
                Table[Core.core.letter] = s;
            }
            Code(Core.left, Table, s + "0");
            Code(Core.right, Table, s + "1");
        }

        static void Main()
        {
            List<Table_Item> Huffman_Table = new List<Table_Item>();
            Dictionary<char, int> Huffman_Table_Temp = new Dictionary<char, int>();
            Dictionary<char, string> Code_Table = new Dictionary<char, string>();
            List<Tree> nodes = new List<Tree>();
            Tree Huffman_tree;
            double entropy = 0;
            string input = "";
            string output = "";

            using (FileStream fin = new FileStream(@"D:\lab_PSU\Information_theory\1\1\bin\Debug\net8.0\input.txt", FileMode.Open))
            {
                byte[] buffer = new byte[fin.Length];
                fin.Read(buffer, 0, buffer.Length);
                input = System.Text.Encoding.Default.GetString(buffer);
            }
            if (input == "")
            {
                Console.Clear();
                Console.WriteLine("Файл пустой.");
                return;
            }

           
            foreach (char c in input)
            {
                if (c == '\n' || c == '\r')
                {
                    continue;
                }
                if (Huffman_Table_Temp.ContainsKey(c))
                {
                    Huffman_Table_Temp[c]++;
                }
                else
                {
                    Huffman_Table_Temp[c] = 1; 
                }
            }

            
            foreach (var item in Huffman_Table_Temp)
            {
                Table_Item temp = new Table_Item();
                temp.num_friq = item.Value; 
                temp.letter = item.Key;
                Huffman_Table.Add(temp);
            }

            var Sorted_Table = from h in Huffman_Table
                               orderby h.num_friq ascending, h.letter
                               select h;
            Huffman_Table = Sorted_Table.ToList();

           
            foreach (var item in Huffman_Table)
            {
                Tree node = new Tree();
                node.core = item;
                node.core_friq = item.num_friq;
                nodes.Add(node);
            }

            
            while (nodes.Count > 1)
            {
                Tree temp = new Tree();
                temp.left = nodes[0];
                temp.right = nodes[1];
                temp.core_friq = nodes[0].core_friq + nodes[1].core_friq;

                nodes.RemoveAt(0);
                nodes.RemoveAt(0);
                nodes.Add(temp);

                nodes = nodes.OrderBy(n => n.core_friq).ToList();
            }

            Huffman_tree = nodes[0];
            string temp_ss = "";

            Code(Huffman_tree, Code_Table, temp_ss);
                    
            foreach (char item in input)
            {
                if (item == '\n' || item == '\r')
                {
                    continue;
                }
                output += Code_Table[item];
            }

            using (FileStream fout = new FileStream(@"D:\lab_PSU\Information_theory\1\1\bin\Debug\net8.0\output.txt", FileMode.Create))
            {
                byte[] buffer = System.Text.Encoding.Default.GetBytes(output);
                fout.Write(buffer, 0, buffer.Length);
            }

            
            Dictionary<string, char> Reverse_Code_Table = new Dictionary<string, char>();
            foreach (var item in Code_Table)
            {
                Reverse_Code_Table[item.Value] = item.Key;
            }
                        
            string decodedOutput = Decode(output, Reverse_Code_Table);
            
            Console.WriteLine("Исходный текст: " + input);
            Console.WriteLine("Полученный код: " + output);
            Console.WriteLine("Декодированный текст: " + decodedOutput);
                        
            foreach (var item in Huffman_Table)
            {
                double temp = Convert.ToDouble(item.num_friq) / input.Length;
                entropy += (temp * Math.Log2(temp));
            }
            entropy *= -1;

            Console.WriteLine("Энтропия: " + entropy);
            Console.WriteLine("Таблица символов: ");
            foreach (var item in Code_Table)
            {
                Console.WriteLine(item.Key + ":  " + item.Value);
            }

            Console.WriteLine("Частота: ");
            foreach (var item in Huffman_Table)
            {
                Console.WriteLine(item.letter + ":  " + item.num_friq);
            }
        }
    }
}