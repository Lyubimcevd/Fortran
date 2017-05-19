using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;

namespace DPPZOT
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void textBox3_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == 13)
            {
                MessageBox.Show("Установите бумагу");
                StreamWriter SW = new StreamWriter("tmp.txt");
                SW.WriteLine(textBox1.Text);
                SW.WriteLine(textBox2.Text);
                SW.WriteLine(textBox3.Text);
                SW.Close();
                SW = new StreamWriter("tmp.bat");
                SW.WriteLine("dppzotmain.exe");
                SW.WriteLine("del tmp.txt");
                SW.WriteLine("del tmp.bat");
                SW.Close();
                this.Close();
                Process.Start("tmp.bat");
            }
        }
    }
}
