using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;

namespace WorkOverAchiever
{
    public sealed partial class DetailsWindow : Window
    {
        public DetailsWindow()
        {
            InitializeComponent();

            // Ensure this window uses the same RequestedTheme as the application so ThemeResource lookups resolve for light/dark
            try
            {
                if (Application.Current is Application app)
                {
                    // Apply theme to the root grid if available
                    if (this.Content is FrameworkElement fe)
                    {
                        fe.RequestedTheme = app.RequestedTheme == ApplicationTheme.Dark ? ElementTheme.Dark : ElementTheme.Light;
                    }
                }
            }
            catch
            {
                // ignore failures
            }
        }

        private void DetailsClearButton_Click(object sender, RoutedEventArgs e)
        {
            DetailsResultTextBlock.Text = string.Empty;
        }

        private void DetailsCalculateButton_Click(object sender, RoutedEventArgs e)
        {
            DetailsResultTextBlock.Text = "Details Calculate clicked - no calculation implemented yet.";
        }
    }
}