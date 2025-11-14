#include "hw7.h"

// Helper function to check if a character is an alphabetic matrix name
int is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

// Inserts matrix into BST and sorts order by name
bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *new_node = malloc(sizeof(bst_sf));
        new_node->mat = mat;
        new_node->left_child = NULL;
        new_node->right_child = NULL;
        return new_node;
    }
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    return root;
}

// Searches for matrix by name
matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;
    }
    
    if (name == root->mat->name) {
        return root->mat;
    } else if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

// Frees all nodes and matrices by post order traversal
void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

// Matrix addition by allocation memory for result matrix and adding elements
matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    int num_rows = (int)mat1->num_rows;
    int num_cols = (int)mat1->num_cols;
    int total = num_rows * num_cols;
    matrix_sf *result = malloc(sizeof(matrix_sf) + total * sizeof(int));
    result->name = '?';
    result->num_rows = num_rows;
    result->num_cols = num_cols;
    for (int i = 0; i < total; i++) {
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    
    return result;
}

// Matrix multiplication by allocating memory for result matrix and performing Z[i][j] = sum(X[i][k] * Y[k][j]
matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    int m = (int)mat1->num_rows;
    int n = (int)mat1->num_cols;
    int p = (int)mat2->num_cols;
    matrix_sf *result = malloc(sizeof(matrix_sf) + m * p * sizeof(int));
    result->name = '?';
    result->num_rows = m;
    result->num_cols = p;
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < p; j++) {
            int sum = 0;
            for (int k = 0; k < n; k++) {
                sum += mat1->values[i * n + k] * mat2->values[k * p + j];
            }
            result->values[i * p + j] = sum;
        }
    }
    
    return result;
}

// Matrix transpose by allocating memory. result [i][j] = mat[i][j]
matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    int num_rows = (int)mat->num_rows;
    int num_cols = (int)mat->num_cols;
    matrix_sf *result = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    result->name = '?';
    result->num_rows = num_cols;
    result->num_cols = num_rows;
    for (int i = 0; i < num_rows; i++) {
        for (int j = 0; j < num_cols; j++) {
            result->values[j * num_rows + i] = mat->values[i * num_cols + j];
        }
    }
    
    return result;
}

// Create matrix from string
matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p = expr;
    while (*p == ' ') 
        p++;
    int num_rows = 0;
    while (*p >= '0' && *p <= '9') {
        num_rows = num_rows * 10 + (*p - '0');
        p++;
    }
    while (*p == ' ') 
        p++;
    int num_cols = 0;
    while (*p >= '0' && *p <= '9') {
        num_cols = num_cols * 10 + (*p - '0');
        p++;
    }
    matrix_sf *mat = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    mat->name = name;
    mat->num_rows = num_rows;
    mat->num_cols = num_cols;
    for (int i = 0; i < num_rows * num_cols; i++) {
        mat->values[i] = 0;
    }
    while (*p && *p != '[') 
        p++; // skips to '['
    if (*p == '[') 
        p++; // skips '['
    int idx = 0;
    while (*p && idx < num_rows * num_cols) {
        while (*p == ' ' || *p == ';') 
            p++; // skips whitespace and semicolons
        if (*p == ']' || *p == '\0' || *p == '\n') 
            break;
        int value = 0;
        int negative = 0;
        if (*p == '-') {
            negative = 1;
            p++;
        }
        while (*p >= '0' && *p <= '9') {
            value = value * 10 + (*p - '0');
            p++;
        }
        if (negative) value = -value;
        mat->values[idx++] = value;
    }
    
    return mat;
}

// Helper to duplicate a matrix (reusing copy_matrix)
matrix_sf* clone_matrix_sf(const matrix_sf *src) {
    if (src == NULL) {
        return NULL;
    }
    matrix_sf *copy = copy_matrix(src->num_rows, src->num_cols, (int*)src->values);
    copy->name = src->name;
    return copy;
}

// Infix to postfix conversion
char* infix2postfix_sf(char *infix) {
    char *postfix = malloc(strlen(infix) * 2 + 2);
    char stack[256];
    int top = -1;
    int pos = 0;
    for (int i = 0; infix[i]; i++) {
        char c = infix[i];
        if (c == ' ') 
            continue;
        if (is_alpha(c)) {
            postfix[pos++] = c;
        }
        else if (c == '(') {
            stack[++top] = c;
        }
        else if (c == ')') {
            while (top >= 0 && stack[top] != '(') {
                postfix[pos++] = stack[top--];
            }
            if (top >= 0) top--; // pop '('
        }
        else if (c == '\'') {
            postfix[pos++] = c;
        }
        else if (c == '+' || c == '*') {
            while (top >= 0 && stack[top] != '(') {
                char top_op = stack[top];
                if (c == '+') {
                    postfix[pos++] = stack[top--];
                }
                else if (c == '*' && top_op == '*') {
                    postfix[pos++] = stack[top--];
                } else {
                    break;
                }
            }
            stack[++top] = c;
        }
    }
    while (top >= 0) {
        postfix[pos++] = stack[top--];
    }
    postfix[pos] = '\0';
    return postfix;
}

// Helper to check if a matrix is from the BST (alphabetic uppercase name)
int is_bst_matrix(const matrix_sf *mat) {
    return mat->name >= 'A' && mat->name <= 'Z';
}

// Evaluate postfix expression
matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char *postfix = infix2postfix_sf(expr);
    matrix_sf *stack[256];
    int top = -1;
    for (int i = 0; postfix[i]; i++) {
        char c = postfix[i];
        if (is_alpha(c)) {
            matrix_sf *mat = find_bst_sf(c, root);
            stack[++top] = mat;
        }
        else if (c == '\'') {
            matrix_sf *operand = stack[top--];
            matrix_sf *result = transpose_mat_sf(operand);
            if (!is_bst_matrix(operand)) {
                free(operand);
            }
            
            stack[++top] = result;
        }
        else if (c == '*') {
            matrix_sf *op2 = stack[top--];
            matrix_sf *op1 = stack[top--];
            matrix_sf *result = mult_mats_sf(op1, op2);
            if (!is_bst_matrix(op1)) free(op1);
            if (!is_bst_matrix(op2)) free(op2);
            stack[++top] = result;
        }
        else if (c == '+') {
            matrix_sf *op2 = stack[top--];
            matrix_sf *op1 = stack[top--];
            matrix_sf *result = add_mats_sf(op1, op2);
            if (!is_bst_matrix(op1)) free(op1);
            if (!is_bst_matrix(op2)) free(op2);
            stack[++top] = result;
        }
    }
    matrix_sf *result = stack[top];
    result->name = name;
    
    free(postfix);
    return result;
}

// Execute script from file
matrix_sf *execute_script_sf(char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) return NULL;
    
    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    
    while ((read = getline(&line, &len, file)) != -1) {
        if (line[0] == '\n') 
            continue; // skip empty lines
        char mat_name = line[0];
        char *equals = strchr(line, '=');
        if (!equals) 
            continue; // finds = sign
        
        char *rhs = equals + 1;
        while (*rhs == ' ') 
            rhs++; // skips space after =
        if (*rhs >= '0' && *rhs <= '9') {
            matrix_sf *mat = create_matrix_sf(mat_name, rhs);
            root = insert_bst_sf(mat, root);
            last_matrix = mat;
        } else {
            char *newline = strchr(rhs, '\n');
            if (newline) *newline = '\0';
            
            matrix_sf *mat = evaluate_expr_sf(mat_name, rhs, root);
            root = insert_bst_sf(mat, root);
            last_matrix = mat;
        }
    }
    matrix_sf *result = clone_matrix_sf(last_matrix);
    free(line);
    fclose(file);
    free_bst_sf(root);
    return result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (int i = 0; i < (int)(mat->num_rows*mat->num_cols); i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
