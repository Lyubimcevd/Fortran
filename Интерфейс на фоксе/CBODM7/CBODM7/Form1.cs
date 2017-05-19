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

namespace CBODM7
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void comboBox1_TextChanged(object sender, EventArgs e)
        {
            StreamWriter SW = new StreamWriter("tmp.txt");
            SW.Write(comboBox1.SelectedIndex);
            SW.Close();
            SW = new StreamWriter("tmp.bat");
            SW.WriteLine("cbodm7main.exe");
            SW.WriteLine("del tmp.txt");
            SW.WriteLine("del tmp.bat");
            SW.Close();
            this.Close();
            Process.Start("tmp.bat");
        }
    }
}
