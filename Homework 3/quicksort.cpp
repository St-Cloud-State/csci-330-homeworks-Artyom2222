#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Partition function
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Choosing last element as pivot
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

// Iterative QuickSort using a stack
void quickSortIterative(vector<int>& arr) {
    stack<pair<int, int>> s;
    s.push({0, arr.size() - 1});

    while (!s.empty()) {
        int low = s.top().first;
        int high = s.top().second;
        s.pop();

        if (low < high) {
            int pivot = partition(arr, low, high);

            if (pivot - 1 > low) {
                s.push({low, pivot - 1});
            }
            if (pivot + 1 < high) {
                s.push({pivot + 1, high});
            }
        }
    }
}

// Main function to test the implementation
int main() {
    vector<int> arr = {10, 7, 8, 9, 1, 5};
    quickSortIterative(arr);

    cout << "Sorted array: ";
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;

    return 0;
}
