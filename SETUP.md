## **Set-up Information**

Follow these steps to set up the environment for this repository:

---

### **1. Download Python**
- Download and install **Python 3.10 or higher** from [https://www.python.org/downloads/](https://www.python.org/downloads/).
- **Make sure to check the box to add Python to your system PATH** during installation. This will allow you to use Python from the command line.

---

### **2. Download VS Code**
- Download and install Visual Studio Code (VS Code) from [https://code.visualstudio.com/](https://code.visualstudio.com/).
- VS Code is recommended for this repository because of its extensions and Python support.
- To work with Jupyter notebooks in VS Code, install the `Jupyter` extension from the Extensions Marketplace.

---

### **3. Install Git**
- Download and install Git from [https://git-scm.com/](https://git-scm.com/).
- Git is required to clone the repository. Follow the installation instructions on the website to set up Git on your system.
- After installation, verify it by running:
  ```sh
  git --version
  ```

---

### **4. Clone the GitHub Repository**
- Open a terminal (**Command Prompt** on Windows, **Terminal** on macOS/Linux).
- Navigate to the folder where you want to clone the repository using `cd`:
  ```sh
  cd path/to/your/folder
  ```
- Clone the repository:
  ```sh
  git clone https://github.com/cathymacpherson/motor-skills-neurodiverse-children.git
  ```

---

### **5. Create and Activate a Virtual Environment**
A virtual environment helps isolate dependencies for this repository.

- Open a terminal (**Command Prompt on Windows, Terminal on macOS/Linux**).
- Navigate to the cloned repository:
  ```sh
  cd motor-skills-neurodiverse-children
  ```
- Create a virtual environment:
  ```sh
  python -m venv .venv
  ```
- **Activate the virtual environment**:
  - **On Windows**:
    ```sh
    .\.venv\Scripts\activate
    ```
  - **On macOS/Linux**:
    ```sh
    source .venv/bin/activate
    ```

---

### **6. Install Dependencies**
Once the virtual environment is activated, install the required dependencies:
```sh
pip install -r requirements.txt
```

---

### **7. Verify the Installation**
Run these commands to check if everything is set up correctly:

```sh
python --version
pip --version
```

---

### **8. Select the Python Interpreter in VS Code**
To ensure VS Code uses your virtual environment:

1. Open the repository folder in VS Code
2. Press `Ctrl+Shift+P` (Windows/Linux) or `Cmd+Shift+P` (macOS) to open the Command Palette
3. Type **"Python: Select Interpreter"** and select it
4. Choose the interpreter from your `.venv` folder (e.g., `.venv\Scripts\python.exe` on Windows)

---

### **9. Run the Notebook**
1. Open `machine-learning.ipynb` in VS Code
2. Ensure the kernel in the top-right corner shows your `.venv` environment
3. Click **"Run All"** or run cells individually using the play button

If you encounter any issues, please refer to the official documentation for Python, VS Code, or Git for help troubleshooting.