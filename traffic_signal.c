#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef int int32_t;
typedef unsigned int uint32_t;

#define MAX_PATH_LENGTH 50
#define BASE_PATH "/sys/class/gpio/gpio"

static void set_gpio_value(const char *path, const char *value) {
    int32_t fd; // File descriptor
    
    fd = open(path, O_WRONLY);
    (void)write(fd, value, strlen(value));
    (void)close(fd);
}

static void set_gpio_direction_output(const char *gpi_pin, const char *dir) {
    int32_t fd_gpi_pin = open(gpi_pin, O_WRONLY); // File descriptor
    (void)write(fd_gpi_pin, dir, strlen(dir));
    (void)close(fd_gpi_pin);
}

void configure_traffic_light(const char *red_path, const char *yellow_path, const char *green_path, const char *state) {
    set_gpio_value(red_path, state[0] == '1' ? "1" : "0");
    set_gpio_value(yellow_path, state[1] == '1' ? "1" : "0");
    set_gpio_value(green_path, state[2] == '1' ? "1" : "0");
}

int main(void) {
    char LED1_RED_PATH[MAX_PATH_LENGTH], LED1_YELLOW_PATH[MAX_PATH_LENGTH], LED1_GREEN_PATH[MAX_PATH_LENGTH]; // Paths for the first traffic light
    char LED2_RED_PATH[MAX_PATH_LENGTH], LED2_YELLOW_PATH[MAX_PATH_LENGTH], LED2_GREEN_PATH[MAX_PATH_LENGTH]; // Paths for the second traffic light
    float green_sig_time; // Green signal time in minutes
    uint32_t yellow_sig_time = 5; // Yellow signal time in seconds
    uint32_t all_red_time = 2;   // All red time in seconds
    uint32_t pin_number; // GPIO pin number

    // Get GPIO pin numbers from the user and construct the paths
    printf("Enter GPIO pin number for LED1 Red: ");
    scanf("%u", &pin_number);
    snprintf(LED1_RED_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter GPIO pin number for LED1 Yellow: ");
    scanf("%u", &pin_number);
    snprintf(LED1_YELLOW_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter GPIO pin number for LED1 Green: ");
    scanf("%u", &pin_number);
    snprintf(LED1_GREEN_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter GPIO pin number for LED2 Red: ");
    scanf("%u", &pin_number);
    snprintf(LED2_RED_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter GPIO pin number for LED2 Yellow: ");
    scanf("%u", &pin_number);
    snprintf(LED2_YELLOW_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter GPIO pin number for LED2 Green: ");
    scanf("%u", &pin_number);
    snprintf(LED2_GREEN_PATH, MAX_PATH_LENGTH, "%s%d/value", BASE_PATH, pin_number);

    printf("Enter green signal time in minutes (can be a fraction): ");
    scanf("%f", &green_sig_time);

    /* Set the directions of the GPIO pins for the LEDs */ 
    set_gpio_direction_output(LED1_RED_PATH, "out");
    set_gpio_direction_output(LED1_YELLOW_PATH, "out");
    set_gpio_direction_output(LED1_GREEN_PATH, "out");
    set_gpio_direction_output(LED2_RED_PATH, "out");
    set_gpio_direction_output(LED2_YELLOW_PATH, "out");
    set_gpio_direction_output(LED2_GREEN_PATH, "out");

    while (1) {
        /* Set the first traffic light to green and the second to red */ 
        configure_traffic_light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "001");
        configure_traffic_light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "100");
        
        (void)sleep((uint32_t)(green_sig_time * 60)); /* Wait for green signal time */ 

        /* Set the first traffic light to yellow */ 
        configure_traffic_light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "010");

        (void)sleep(yellow_sig_time); /* Wait for 5 seconds */ 

        /* Set the first traffic light to red */ 
        configure_traffic_light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "100");

        /* Set the second traffic light to red and yellow */ 
        configure_traffic_light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "110");

        (void)sleep(all_red_time); /* Wait for 2 seconds with both sides red */

        /* Set the second traffic light to green */ 
        configure_traffic_light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "001");

        (void) sleep((uint32_t)(green_sig_time * 60)); /* Wait for green signal time */ 

        /* Set the second traffic light to yellow */ 
        configure_traffic_light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "010");

        (void)sleep(yellow_sig_time); /* Wait for 5 seconds */ 

        /* Set the second traffic light to red */ 
        configure_traffic_light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "100");

        /* Set the first traffic light to red and yellow */ 
        configure_traffic_light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "110");

        (void)sleep(all_red_time); /* Wait for 2 seconds with both sides red */

        /* Set the first traffic light to green */ 
        configure_traffic_light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "001");
    }

    return 0;
}
